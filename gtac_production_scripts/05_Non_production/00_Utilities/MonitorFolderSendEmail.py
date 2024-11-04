
#%%
import os
import smtplib
from email.mime.text import MIMEText
import time


#%%

#Set the model and time frame you want to monitor. Include the name of the machine processing. 
timeFrame = "MidCentury"
model = "MPI-ESM1-2-HR"
machine = "LPP2Z3"

#Set the path to folder 
folder_path = fr"X:\35_NOGA\02FEIS\02Analysis\ClimateRefugia\02Data\ProcessedData\Test5PcaWithCMIP6V2\{timeFrame}\{model}\RefugiaMaps\ByBps"


#Function to send an email 
def send_email():
    sender = "replaceWithEmail@gmail.com" # I used gmail because you'll need an "app password and I wasn't sure how to get that for the FS email"
    recipient = "2107161713@vtext.com" # replace with your phone number and SMS gateway, or alternately replace with email address 
    password = "xxxx xxxx xxxx xxxx" # replace with app pasword for gmail 

    subject = f"{machine} FINISHED {timeFrame} {model}" 
    body = f"PCA done for {timeFrame} {model}"

    msg = MIMEText(body)
    msg['Subject'] = subject
    msg['From'] = sender
    msg['To'] = recipient

    with smtplib.SMTP_SSL('smtp.gmail.com', 465) as server:
        server.login(sender, password)
        server.sendmail(sender, recipient, msg.as_string())

#Function to monitor file count in folder 
def monitor_folder(folder_path):
    file_count = len([name for name in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, name))])
    print(f"File count: {file_count}")
    return file_count


#Run the monitor in a loop 
while True:
    file_count = monitor_folder(folder_path)
    if file_count >= 399:
        send_email()
        print("Email sent")
        break
    time.sleep(1000) #time in seconds, will concurrently check 
    


# %%
