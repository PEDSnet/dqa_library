import airflow
from datetime import timedelta, datetime
from airflow import DAG
from airflow.utils.dates import days_ago
from airflow.operators.bash import BashOperator
import os
import shutil
import glob
from airflow.decorators import dag, task, task_group
from airflow.models.param import Param

cwd = '/data/airflow/dags/dqa_library/'
default_args = {
		'owner': 'wieandk',
		'retries': 0,
        'schedule': "None",
		'retry_delay': timedelta(minutes=0.5),
}

dag = DAG(
    dag_id = 'dqa_library',
    default_args = default_args,
    schedule_interval=None,
    start_date=datetime(2023, 8, 8), 
    template_searchpath = ['/data/airflow/dags/dqa_library/scripts/site/'],
    description='Network DQA checks',
    params = {
         'user': Param(None, type=['null','string']),
         'password': Param(None, type=['null','string']),
         'site': Param(None, type=['null','string']),
         'db_current': Param(None, type=['null','string']),
         'db_prev': Param(None, type=['null','string']),
         'results_schema': Param(None, type=['null','string']),
         # 'fot_months': Param(None, type=['null','integer']),
        }
)


# Step 1 - Precompute Tables
precompute_tables = BashOperator(
        task_id = 'precompute_tables' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/1_precompute_tables.R ',
        dag = dag)

# Step 2 - Run FOT (Split)
fot_1 = BashOperator(
		    task_id = 'facts_over_time_1' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_fot1.R ',
        dag = dag)
        
fot_2 = BashOperator(
		    task_id = 'facts_over_time_2' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_fot2.R ',
        dag = dag)
        
fot_3 = BashOperator(
		    task_id = 'facts_over_time_3' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_fot3.R ',
        dag = dag)
        
fot_4 = BashOperator(
		    task_id = 'facts_over_time_4' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_fot4.R ',
        dag = dag)
        
fot_5 = BashOperator(
		    task_id = 'facts_over_time_5' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_fot5.R ',
        dag = dag)

# Step 3 - Run PF (Split)
person_facts_all = BashOperator(
		    task_id = 'person_facts_all' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_pf_all.R ',
        dag = dag)

person_facts_ip = BashOperator(
		    task_id = 'person_facts_ip' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_pf_ip.R ',
        dag = dag)
        
person_facts_op = BashOperator(
		    task_id = 'person_facts_op' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_pf_op.R ',
        dag = dag)
        
person_facts_ed = BashOperator(
		    task_id = 'person_facts_ed' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_pf_ed.R ',
        dag = dag)

# Step 4 - VC/VS & DCON
vocab_valueset_conf = BashOperator(
		    task_id = 'vocab_valueset_conf' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/3_driver_vc_vs.R ',
        dag = dag)

domain_concordance = BashOperator(
		    task_id = 'domain_concordance' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/3_driver_dcon.R ',
        dag = dag)

# Step 5 - DC, MF, UC, BMC, ECP
data_cycle_changes = BashOperator(
		    task_id = 'data_cycle_changes' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_dc.R ',
        dag = dag)
        
mf_visitid = BashOperator(
		    task_id = 'mf_visitid' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/2_driver_mf.R ',
        dag = dag)

unmapped_concepts = BashOperator(
		    task_id = 'unmapped_concepts' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/3_driver_uc.R ',
        dag = dag)
        
best_mapped_concepts = BashOperator(
		    task_id = 'best_mapped_concepts' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/3_driver_bmc.R ',
        dag = dag)
        
best_mapped_concepts = BashOperator(
		    task_id = 'expected_concepts' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/3_driver_ecp.R ',
        dag = dag)

# Step 6 - Remove Precomputed Tables
remove_precomp = BashOperator(
		    task_id = 'remove_precomp' ,
        bash_command = f'{cwd}scripts/run_r.sh {cwd}scripts/4_remove_precomp.R ',
        dag = dag)

precompute_tables >> fot_1 >> expected_concepts >> person_facts_ed >> data_cycle_changes >> remove_precomp
precompute_tables >> fot_2 >> person_facts_ip >> vocab_valueset_conf >> mf_visitid >> remove_precomp
precompute_tables >> fot_3 >> person_facts_op >> domain_concordance >> unmapped_concepts >> remove_precomp
precompute_tables >> fot_4 >> person_facts_all >> best_mapped_concepts >> remove_precomp
precompute_tables >> fot_5 >> remove_precomp

