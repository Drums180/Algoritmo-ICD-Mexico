import os
import pandas as pd


def convert_parquet_to_csv(folder_path):
    print(f"Buscando archivos en {folder_path}")
    for root, dirs, files in os.walk(folder_path):
        for file in files:
            if file.endswith('.parquet'):
                parquet_path = os.path.join(root, file)
                csv_path = parquet_path.replace('.parquet', '.csv')
                print(f"Convirtiendo {parquet_path} a {csv_path}")
                df = pd.read_parquet(parquet_path)
                df.to_csv(csv_path, index=False)
                os.remove(parquet_path)  # Elimina el archivo Parquet
                print(f"Archivo Parquet eliminado: {parquet_path}")


base_folder = '/Users/daviddrums180/Arca Continental/CENSO ARCA/MEXICO/CENSO TRAD/parquets'
convert_parquet_to_csv(base_folder)
