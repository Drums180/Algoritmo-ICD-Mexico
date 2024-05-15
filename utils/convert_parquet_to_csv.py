import os
import pandas as pd
import shutil

def convert_parquet_to_csv(folder_path, base_folder):
    # Recorre recursivamente todas las subcarpetas y archivos en folder_path
    for root, dirs, files in os.walk(folder_path, topdown=False):  # topdown=False para eliminar subdirectorios después
        for file in files:
            if file.endswith('.parquet'):
                parquet_path = os.path.join(root, file)
                csv_file_name = os.path.splitext(file)[0] + '.csv'
                csv_path = os.path.join(base_folder, csv_file_name)
                print(f"Convirtiendo {parquet_path} a {csv_path}")
                df = pd.read_parquet(parquet_path)
                df.to_csv(csv_path, index=False)
                os.remove(parquet_path)  # Elimina el archivo Parquet
                print(f"Archivo Parquet eliminado: {parquet_path}")
        
        # Después de convertir y eliminar archivos Parquet, eliminamos los directorios si están vacíos
        for dir in dirs:
            dir_path = os.path.join(root, dir)
            try:
                os.rmdir(dir_path)  # Intenta eliminar el directorio
                print(f"Directorio eliminado: {dir_path}")
            except OSError:
                pass  # El directorio no está vacío, así que no se elimina

def process_base_folder(base_folder):
    subfolders = ["actual", "scenes", "session"]
    for subfolder in subfolders:
        folder_path = os.path.join(base_folder, subfolder)
        if os.path.exists(folder_path):
            convert_parquet_to_csv(folder_path, folder_path)
        else:
            print(f"La carpeta {folder_path} no existe")

# Define la ruta base manualmente
base_folder = '/Users/daviddrums180/Arca Continental/git_arca/ICD_Mexico/utils/parquets'
print(f"Ruta base: {base_folder}")  # Imprime la ruta base para verificarla
process_base_folder(base_folder)

