import pdfplumber
from scripts.config_loader import config

def extract_full_text(pdf_path):
    """
    Extract all text from a PDF file using pdfplumber.
    
    Args:
        pdf_path: Path to the PDF file
        
    Returns:
        Extracted text as string
    """
    text = ""
    
    try:
        with pdfplumber.open(pdf_path) as pdf:
            for page in pdf.pages:
                extracted = page.extract_text() or ""
                text += extracted
    except Exception as e:
        print(f"Error extracting text from {pdf_path}: {str(e)}")
        
    return text

def extract_partial_text(pdf_path, page_limit=None):
    """
    Extract text up to a certain number of pages.
    
    Args:
        pdf_path: Path to the PDF file
        page_limit: Maximum number of pages to extract (default from config)
        
    Returns:
        Extracted text as string
    """
    # Use page_limit from arguments, or fall back to config
    if page_limit is None:
        page_limit = config.get("pdf.extraction.page_limit_for_metadata", 2)
        
    text = ""
    
    try:
        with pdfplumber.open(pdf_path) as pdf:
            for i, page in enumerate(pdf.pages):
                if i < page_limit:
                    extracted = page.extract_text() or ""
                    text += extracted
                else:
                    break
    except Exception as e:
        print(f"Error extracting partial text from {pdf_path}: {str(e)}")
        
    return text
