#!/bin/bash

# Получаем абсолютный путь к директории скрипта
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Создание необходимых директорий
mkdir -p "${SCRIPT_DIR}/logs/errors" "${SCRIPT_DIR}/results/sequential/2d" "${SCRIPT_DIR}/results/sequential/3d" "${SCRIPT_DIR}/results/parallel/2d" "${SCRIPT_DIR}/results/parallel/3d"

# Функция логирования
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "${SCRIPT_DIR}/logs/execution.log"
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
    local output_dir="${SCRIPT_DIR}/results/sequential/${dim}/${program_name}"
    
    log "Компиляция последовательной версии: $program"
    cd "$program_dir"
    "${SCRIPT_DIR}/dvm" f -s "$program_name.f" 2>>"${SCRIPT_DIR}/logs/errors/${program_name}_seq.log"
    
    if [ $? -eq 0 ]; then
        log "Запуск последовательной версии: $program"
        "${SCRIPT_DIR}/dvm" run "./$program_name" 2>>"${SCRIPT_DIR}/logs/errors/${program_name}_seq.log"
        save_stats "$program_dir" "$output_dir"
    else
        log "ОШИБКА: Не удалось скомпилировать $program"
    fi
}

# Функция запуска параллельной версии
run_parallel() {
    local program="$1"
    local dim="$2"
    local ppn="$3"
    local threads="$4"
    local grid_x="$5"
    local grid_y="$6"
    local grid_z="$7"
    
    local program_dir="$(dirname "$program")"
    local program_name="$(basename "$program" .f)"
    local output_dir="${SCRIPT_DIR}/results/parallel/${dim}/${program_name}/p${ppn}_t${threads}_${grid_x}x${grid_y}x${grid_z}"
    
    update_dvm_config "$ppn" "$threads"
    
    log "Компиляция параллельной версии: $program (PPN=$ppn, Threads=$threads)"
    cd "$program_dir"
    "${SCRIPT_DIR}/dvm" f "$program_name.f" 2>>"${SCRIPT_DIR}/logs/errors/${program_name}_par.log"
    
    if [ $? -eq 0 ]; then
        log "Запуск параллельной версии: $program (Grid: ${grid_x}x${grid_y}x${grid_z})"
        "${SCRIPT_DIR}/dvm" run "$grid_x" "$grid_y" "$grid_z" "./$program_name" 2>>"${SCRIPT_DIR}/logs/errors/${program_name}_par.log"
        save_stats "$program_dir" "$output_dir"
    else
        log "ОШИБКА: Не удалось скомпилировать $program"
    fi
}

# Основной цикл для 2D программ
for program in "${SCRIPT_DIR}/sources/2d"/*.f; do
    if [ -f "$program" ]; then
        # Последовательный запуск
        run_sequential "$program" "2d"
        
        # Параллельные запуски
        for ppn in 1 2 4; do
            for threads in 1 2 4 8; do
                for grid_size in "1 1" "2 2" "4 4"; do
                    read -r x y <<< "$grid_size"
                    run_parallel "$program" "2d" "$ppn" "$threads" "$x" "$y" "1"
                done
            done
        done
    fi
done

# Основной цикл для 3D программ
for program in "${SCRIPT_DIR}/sources/3d"/*.f; do
    if [ -f "$program" ]; then
        # Последовательный запуск
        run_sequential "$program" "3d"
        
        # Параллельные запуски
        for ppn in 1 2 4; do
            for threads in 1 2 4 8; do
                for grid_size in "1 1 1" "2 2 2" "4 4 4"; do
                    read -r x y z <<< "$grid_size"
                    run_parallel "$program" "3d" "$ppn" "$threads" "$x" "$y" "$z"
                done
            done
        done
    fi
done

log "Выполнение всех тестов завершено"
