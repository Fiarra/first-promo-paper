import os
from scripts.config_loader import config

def rename_pdfs_in_folder(folder_path=None, prefix=None):
    """
    Renames all PDFs in a folder to a standardized naming format.
    
    Args:
        folder_path: Directory containing PDFs (default from config)
        prefix: Prefix for renamed files (default from config)
        
    Returns:
        List of full paths to renamed files
    """
    # Use arguments or fall back to config values
    if folder_path is None:
        folder_path = config.get("paths.data_dir")
    
    if prefix is None:
        prefix = config.get("pdf.rename.prefix", "paper")
    
    # Check if renaming is enabled in config
    if not config.get("pdf.rename.enabled", True):
        print("PDF renaming is disabled in config. Returning original filenames.")
        pdf_files = sorted([os.path.join(folder_path, f) for f in os.listdir(folder_path) 
                           if f.lower().endswith(".pdf")])
        return pdf_files
    
    try:
        pdf_files = sorted([f for f in os.listdir(folder_path) if f.lower().endswith(".pdf")])
        renamed_files = []

        for i, filename in enumerate(pdf_files, start=1):
            new_name = f"{prefix}_{i:03}.pdf"
            old_path = os.path.join(folder_path, filename)
            new_path = os.path.join(folder_path, new_name)
            
            # Only rename if the file doesn't already have the correct name
            if filename != new_name:
                os.rename(old_path, new_path)
            
            renamed_files.append(new_path)
        
        return renamed_files
    
    except Exception as e:
        print(f"Error renaming PDFs in {folder_path}: {str(e)}")
        return []