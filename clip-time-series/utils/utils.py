from sepal_ui.scripts import gee as gs
from utils import messages as ms
import time

def custom_wait_for_completion(task_description, output):
    """Wait until the selected process are finished. Display some output information

    Args:
        task_descripsion ([str]) : name of the running tasks
        widget_alert (v.Alert) : alert to display the output messages
    
    Returns: state (str) : final state
    """
    state = 'UNSUBMITTED'
    while not (state == 'COMPLETED' or state =='FAILED'):
        output.add_live_msg(ms.STATUS.format(state))
        time.sleep(5)
                    
        #search for the task in task_list
        for task in task_description:
            current_task = gs.isTask(task)
            if current_task:
                state = current_task.state
                if state == 'RUNNING' or state == 'FAILED': 
                    break
                    
    return state