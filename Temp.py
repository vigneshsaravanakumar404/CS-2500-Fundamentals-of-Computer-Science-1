from PIL import Image
import pytesseract
import pdf2image

def pdf_to_string(pdf_path):
    images = pdf2image.convert_from_path(pdf_path)
    text = ''
    for image in images:
        text += pytesseract.image_to_string(image)
    return text

pdf_path = r"C:\Users\Vigne\Downloads\Elizabeth Eisenstein, “The Unacknowledged Revolution,” in The Printing Press as an Agent of Change_ Communications and Cultural Transformations inEarly Modern Europe. Cambridge_ Cambridge University Press, 1979. .pdf"
pdf_text = pdf_to_string(pdf_path)
print(pdf_text)
