import pdfplumber

def extract_full_text(pdf_path):
    """
    Extract all text from a PDF file using pdfplumber.
    """
    text = ""
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            text += page.extract_text() or ""  # fallback to empty string if no text
    return text

def extract_partial_text(pdf_path, page_limit=2):
    """
    Extract text up to a certain number of pages (default=2).
    """
    text = ""
    with pdfplumber.open(pdf_path) as pdf:
        for i, page in enumerate(pdf.pages):
            if i < page_limit:
                text += page.extract_text() or ""
            else:
                break
    return text
