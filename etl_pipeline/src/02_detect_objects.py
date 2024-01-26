import glob
import os
import sys
import json
import cv2
import torch
from PIL import Image
from array import array


# Inferences with a custom yolov5 model

# model = torch.hub.load('ultralytics/yolov5', 'custom', path='path/to/best.pt')  # local model
# model = torch.hub.load('path/to/yolov5', 'custom', path='path/to/best.pt', source='local')  # local repo
# Model in local repo
model = torch.hub.load('./', 'custom', path='./best.pt', source='local', device = "cpu")
  
model.conf = 0.01  # NMS confidence threshold
model.iou = 0.1  # NMS IoU threshold
model.agnostic = False  # NMS class-agnostic
model.multi_label = False  # NMS multiple labels per box
model.classes = None  # (optional list) filter by class, i.e. = [0, 15, 16] for COCO persons, cats and dogs
model.max_det = 1000  # maximum number of detections per image
model.amp = False  # Automatic Mixed Precision (AMP) inference
  

def detect_objects_from_jpg_images(main_dir):
    input_image_folder = os.path.join(main_dir, "split_images_without_detections/")
    output_image_folder = os.path.join(main_dir, "split_images_with_detections/")
    output_coodinate_folder = os.path.join(main_dir, "split_images_detection_coordinates/")
  
    input_image_subdirectories = os.listdir(input_image_folder)

    for subdirectory in input_image_subdirectories:
        current_input_image_path = os.path.join(input_image_folder, subdirectory)
        current_output_image_path = os.path.join(output_image_folder, subdirectory)
        current_output_coodinate_path = os.path.join(output_coodinate_folder, subdirectory)
        
        if not any(os.scandir(current_output_image_path)):
            images = glob.glob(os.path.join(current_input_image_path, '*.jpg'))
            n_images = len(images)
            results = model(images)
            results.save(save_dir = current_output_image_path, exist_ok = True)

            for element in range(0,n_images):
                basename = os.path.basename(images[element])
                filename = os.path.splitext(basename)[0]
                current_output_coodinate_file = os.path.join(current_output_coodinate_path, f"{filename}.csv")
                results.pandas().xyxy[element].to_csv(current_output_coodinate_file, header=True, index = False)
    
    return None
      

