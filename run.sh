#!/bin/bash

# Получаем абсолютный путь к директории скрипта
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Параметры машины (можно менять или передавать как аргументы)
MAX_CORES=16      # Максимальное количество ядер процессора
MAX_THREADS=32    # Максимальное количество потоков на машине
IS_PPN=0          # Флаг для перебора значений PPN (по умолчанию выключен)
DEVICE_NAME=""    # Название устройства/машины (по умолчанию пусто)

# Проверка аргументов командной строки
if [ "$#" -ge 2 ]; then
    MAX_CORES=$1
    MAX_THREADS=$2
    shift 2
    
    # Проверяем, есть ли дополнительный аргумент is_ppn
    if [ "$#" -ge 1 ] && [ "$1" = "is_ppn" ]; then
        IS_PPN=1
        shift 1
    fi
    
    # Проверяем, есть ли указание имени устройства
    if [ "$#" -ge 2 ] && [ "$1" = "device_name" ]; then
        DEVICE_NAME=$2
        shift 2
    fi
fi

# Определяем пути для логов и результатов в зависимости от имени устройства
if [ -n "$DEVICE_NAME" ]; then
    LOGS_DIR="${SCRIPT_DIR}/${DEVICE_NAME}/logs"
    RESULTS_DIR="${SCRIPT_DIR}/${DEVICE_NAME}/results"
else
    LOGS_DIR="${SCRIPT_DIR}/logs"
    RESULTS_DIR="${SCRIPT_DIR}/results"
fi

# Создание необходимых директорий для всех измерений (1D - 4D)
for dim in 1d 2d 3d 4d; do
    mkdir -p "${LOGS_DIR}/errors" 
    mkdir -p "${RESULTS_DIR}/sequential/${dim}" 
    mkdir -p "${RESULTS_DIR}/parallel/${dim}"
done

# Функция логирования
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "${LOGS_DIR}/execution.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"  # Вывод в консоль для отслеживания
}

# Функция обновления конфига DVM
update_dvm_config() {
    local ppn="$1"
    local threads="$2"
    local temp_file="${SCRIPT_DIR}/dvm.tmp"
    
    sed -e "s/export DVMH_PPN=.*/export DVMH_PPN='$ppn'/" \
        -e "s/export DVMH_NUM_THREADS=.*/export DVMH_NUM_THREADS='$threads'/" \
        "${SCRIPT_DIR}/dvm" > "$temp_file"
    mv "$temp_file" "${SCRIPT_DIR}/dvm"
    chmod +x "${SCRIPT_DIR}/dvm"
}

# Функция сохранения статистики
save_stats() {
    local program_dir="$1"    # Директория, где находится программа
    local output_dir="$2"     # Директория для сохранения результатов
    
    # Создаем директорию для результатов
    mkdir -p "$output_dir"
    
    # Ищем .gz+ файл и обрабатываем его
    if [ -f "$program_dir/sts.gz+" ]; then
        # Запускаем pa и сохраняем результат в stat.txt
        "${SCRIPT_DIR}/dvm" pa "$program_dir/sts.gz+" "$program_dir/stat.txt"
        # Перемещаем stat.txt в директорию с результатами
        mv "$program_dir/stat.txt" "$output_dir/"
        # Опционально можно также сохранить исходный .gz+ файл
        mv "$program_dir/sts.gz+" "$output_dir/"
        log "Создан анализ производительности в $output_dir/stat.txt"
    else
        log "ПРЕДУПРЕЖДЕНИЕ: Файл sts.gz+ не найден в $program_dir"
    fi
}

# Функция запуска последовательной версии
run_sequential() {
    local program="$1"
    local dim="$2"
    local program_dir="$(dirname "$program")"
    local program_name="$(basename "$program" .f)"
    local output_dir="${RESULTS_DIR}/sequential/${dim}/${program_name}"
    
    log "Компиляция последовательной версии: $program"
    cd "$program_dir"
    "${SCRIPT_DIR}/dvm" f -s "./$program_name.f" 2>>"${LOGS_DIR}/errors/${program_name}_seq.log"
    
    if [ $? -eq 0 ]; then
        log "Запуск последовательной версии: $program"
        "${SCRIPT_DIR}/dvm" run "./$program_name" 2>>"${LOGS_DIR}/errors/${program_name}_seq.log"
        save_stats "$program_dir" "$output_dir"
    else
        log "ОШИБКА: Не удалось скомпилировать $program"
    fi
}

# Функция запуска параллельной версии с динамическим числом измерений
run_parallel() {
    local program="$1"
    local dim="$2"
    local ppn="$3"
    local threads="$4"
    shift 4
    local grid_dims=("$@")
    
    local program_dir="$(dirname "$program")"
    local program_name="$(basename "$program" .f)"
    
    # Создаем строку с grid размерами для названия директории
    local grid_str=""
    for val in "${grid_dims[@]}"; do
        grid_str="${grid_str}${val}x"
    done
    grid_str="${grid_str%x}"  # Удаляем последний x
    
    local output_dir="${RESULTS_DIR}/parallel/${dim}/${program_name}/p${ppn}_t${threads}_${grid_str}"
    
    update_dvm_config "$ppn" "$threads"
    
    log "Компиляция параллельной версии: $program (PPN=$ppn, Threads=$threads)"
    cd "$program_dir"
    "${SCRIPT_DIR}/dvm" f "$program_name.f" 2>>"${LOGS_DIR}/errors/${program_name}_par.log"
    
    if [ $? -eq 0 ]; then
        log "Запуск параллельной версии: $program (Grid: ${grid_str})"
        
        # Динамически формируем аргументы для запуска
        local run_args=()
        for val in "${grid_dims[@]}"; do
            run_args+=("$val")
        done
        run_args+=("./$program_name")
        
        "${SCRIPT_DIR}/dvm" run "${run_args[@]}" 2>>"${LOGS_DIR}/errors/${program_name}_par.log"
        save_stats "$program_dir" "$output_dir"
    else
        log "ОШИБКА: Не удалось скомпилировать $program"
    fi
}

# Функция для генерации всех возможных комбинаций размера сетки для заданной размерности
generate_grid_combinations() {
    local dims="$1"
    local values=("$@")
    
    # Удаляем первый аргумент (dims)
    values=("${values[@]:1}")
    
    case "$dims" in
        1)
            for x in "${values[@]}"; do
                echo "$x"
            done
            ;;
        2)
            for x in "${values[@]}"; do
                for y in "${values[@]}"; do
                    echo "$x $y"
                done
            done
            ;;
        3)
            for x in "${values[@]}"; do
                for y in "${values[@]}"; do
                    for z in "${values[@]}"; do
                        echo "$x $y $z"
                    done
                done
            done
            ;;
        4)
            for x in "${values[@]}"; do
                for y in "${values[@]}"; do
                    for z in "${values[@]}"; do
                        for w in "${values[@]}"; do
                            echo "$x $y $z $w"
                        done
                    done
                done
            done
            ;;
    esac
}

# Функция для обработки программы по измерениям
process_program() {
    local program="$1"
    local dim_num="$2"  # число измерений (1-4)
    local dim="${dim_num}d"
    
    # Последовательный запуск
    run_sequential "$program" "$dim"
    
    # Определяем значения PPN в зависимости от флага IS_PPN
    local ppn_values=()
    if [ "$IS_PPN" -eq 1 ]; then
        # Если флаг установлен, перебираем различные значения PPN
        for ((i=1; i<=MAX_CORES; i*=2)); do
            ppn_values+=($i)
        done
    else
        # Если флаг не установлен, используем только одно значение PPN (1)
        ppn_values=(1)
    fi
    
    local thread_values=()
    for ((i=1; i<=MAX_THREADS; i*=2)); do
        thread_values+=($i)
    done
    
    # Значения для размеров сетки
    local grid_values=(1 2 4)
    
    # Параллельные запуски
    for ppn in "${ppn_values[@]}"; do
        for threads in "${thread_values[@]}"; do
            # Генерация комбинаций размера сетки для данной размерности
            readarray -t grid_combinations < <(generate_grid_combinations "$dim_num" "${grid_values[@]}")
            
            for grid_combination in "${grid_combinations[@]}"; do
                # Преобразуем строку с пробелами в массив
                read -ra grid_dims <<< "$grid_combination"
                run_parallel "$program" "$dim" "$ppn" "$threads" "${grid_dims[@]}"
            done
        done
    done
}

# Проверка наличия и обработка программ для всех размерностей
for dim_num in {1..4}; do
    dim="${dim_num}d"
    source_dir="${SCRIPT_DIR}/sources/${dim}"
    
    # Проверяем, существует ли директория
    if [ -d "$source_dir" ]; then
        for program in "$source_dir"/*.f; do
            if [ -f "$program" ]; then
                process_program "$program" "$dim_num"
            fi
        done
    else
        log "Директория $source_dir не существует, пропускаем тесты для ${dim}"
    fi
done

log "Выполнение всех тестов завершено. Использованы параметры машины: MAX_CORES=$MAX_CORES, MAX_THREADS=$MAX_THREADS, IS_PPN=$IS_PPN, DEVICE_NAME=$DEVICE_NAME"