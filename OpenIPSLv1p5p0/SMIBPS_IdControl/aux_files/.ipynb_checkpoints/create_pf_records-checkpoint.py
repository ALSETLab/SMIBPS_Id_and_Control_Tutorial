import os
from .generate_component_list import *

def create_pf_records(model_name, model_path):

    '''
    create_pf_records

    DESCRIPTION:

    INPUTS:
    - model_path

    OUTPUTS:
    - void function. Writes '.mo' files i

    LAST MODIFICATION DATE:

    '''
    current_dir = os.getcwd()
    os.chdir(os.path.dirname(model_path))

    # Getting the list of the components of the given '.mo' file
    components = generate_component_list(model_path)

    # Sorting alphabetically the component list
    for comp in components.keys():
        components[comp].sort()

    #############################################################
    ################# CREATING RECORD STRUCTURE #################
    #############################################################

    # Getting current directory
    directory = os.getcwd()

    # Getting absolute path of the 'PF_Data' folder
    pf_data_folder_path = os.path.join(directory, 'PF_Data')

    # Creating the 'PF_Data' folder if it does not exist
    if not os.path.exists(pf_data_folder_path):
        os.makedirs(pf_data_folder_path)

    # Adding the 'PF_Data' reference to the 'package.order' file
    pk_order_path = os.path.join(directory, 'package.order') # Getting the path to the 'package.order' file

    # Checking if PF_Data already is in package.order
    with open(pk_order_path, 'r') as file:
        lines = file.readlines()

    # If it is not, then append 'PF_Data' at the end of the package.order file
    with open(pk_order_path, 'a') as file:
        if 'PF_Data' not in lines:
            file.writelines('\nPF_Data')

    # Creating 'package.mo' file for each model (if it does not exist yet)
    if 'package.mo' not in os.listdir(pf_data_folder_path):
        pack_mo_path = os.path.join(pf_data_folder_path, 'package.mo')
        pack_mo = open(pack_mo_path, "w+")
        pack_mo.write("within {};\n".format(model_name))
        pack_mo.write("package PF_Data\n\n")
        pack_mo.write("end PF_Data;")
        pack_mo.close()

    # Creating 'package.order' file for each model (if it does not exist yet)
    if 'package.order' not in os.listdir(pf_data_folder_path):
        pack_order_path = os.path.join(pf_data_folder_path, 'package.order')
        pack_order = open(pack_order_path, "w+")
        # Writing main record structure
        pack_order.write("PowerFlow_Data\n")
        # Writing templates
        pack_order.write("Bus_Template\n")
        pack_order.write("Loads_Template\n")
        pack_order.write("Trafos_Template\n")
        pack_order.write("Machines_Template\n")
        # Writing subfolders with power flow results
        pack_order.write("Bus_Data\n")
        pack_order.write("Loads_Data\n")
        pack_order.write("Trafos_Data\n")
        pack_order.write("Machines_Data")
        pack_order.close()

    # Creating 'PowerFlow_Data.mo' file for each model (if it does not exist yet)
    if 'PowerFlow_Data.mo' not in os.listdir(pf_data_folder_path):
        pf_data_path = os.path.join(pf_data_folder_path, 'PowerFlow_Data.mo')
        pf_data = open(pf_data_path, "w+")

        # Header of the '.mo' file
        pf_data.write("within {}.PF_Data;\n".format(model_name))
        pf_data.write("record PowerFlow_Data\n")
        pf_data.write("extends Modelica.Icons.Record;\n\n")

        # Bus attribute declaration
        pf_data.write("replaceable record Bus = {}.PF_Data.Bus_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Bus_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Bus bus;\n\n")

        # Loads attribute declaration
        pf_data.write("replaceable record Loads = {}.PF_Data.Loads_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Loads_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Loads loads;\n\n")

        # Trafos attribute declaration
        pf_data.write("replaceable record Trafos = {}.PF_Data.Trafos_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Trafos_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Trafos trafos;\n\n")

        # Machines attribute declaration
        pf_data.write("replaceable record Machines = {}.PF_Data.Machines_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Machines_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Machines machines;\n\n")

        pf_data.write("end PowerFlow_Data;")
        pf_data.close()


    ######################################################
    ################# CREATING TEMPLATES #################
    ######################################################

    ###########################
    ####### BUS TEMPLATE ######
    ###########################

    if 'Bus_Template.mo' not in os.listdir(pf_data_folder_path):

        # Creating the .mo file
        bus_template_path = os.path.join(pf_data_folder_path, 'Bus_Template.mo')
        bus_template = open(bus_template_path, "w+")
        bus_template.write("record Bus_Template\n\n")

        for n_bus, _ in enumerate(components['buses']):

            # Write V, A (and delta for the slack)
            bus_template.write("parameter Real V{};\n".format(n_bus + 1))
            bus_template.write("parameter Real A{};\n\n".format(n_bus + 1))

        bus_template.write("end Bus_Template;")
        bus_template.close()

    ###########################
    ##### LOADS TEMPLATE ######
    ###########################

    if 'Loads_Template.mo' not in os.listdir(pf_data_folder_path):

        # Creating the .mo file
        loads_template_path = os.path.join(pf_data_folder_path, 'Loads_Template.mo')
        loads_template = open(loads_template_path, "w+")
        loads_template.write("record Loads_Template\n\n")

        for n_load, _ in enumerate(components['loads']):

            # Write P,Q
            loads_template.write("parameter Real PL{};\n".format(n_load + 1))
            loads_template.write("parameter Real QL{};\n\n".format(n_load + 1))

        loads_template.write("end Loads_Template;")
        loads_template.close()

    ###########################
    #### MACHINES TEMPLATE ####
    ###########################

    if 'Machines_Template.mo' not in os.listdir(pf_data_folder_path):

        # Creating the .mo file
        machines_template_path = os.path.join(pf_data_folder_path, 'Machines_Template.mo')
        machines_template = open(machines_template_path, "w+")
        machines_template.write("record Machines_Template\n\n")

        for n_gen, _ in enumerate(components['generators']):
            # Write PG, QG
            machines_template.write("parameter Real PG{};\n".format(n_gen + 1))
            machines_template.write("parameter Real QG{};\n\n".format(n_gen + 1))

        machines_template.write("end Machines_Template;")
        machines_template.close()

    ###########################
    ## TRANSFORMERS TEMPLATE ##
    ###########################

    if 'Trafos_Template.mo' not in os.listdir(pf_data_folder_path):

        # Creating the .mo file
        trafos_template_path = os.path.join(pf_data_folder_path, 'Trafos_Template.mo')
        trafos_template = open(trafos_template_path, "w+")
        trafos_template.write("record Trafos_Template\n\n")

        for n_trafo, _ in enumerate(components['trafos']):
            # Write t1, t2
            trafos_template.write("parameter Real t1_trafo_{};\n".format(n_trafo + 1))
            trafos_template.write("parameter Real t2_trafo_{};\n\n".format(n_trafo + 1))

        trafos_template.write("end Trafos_Template;")
        trafos_template.close()

    os.chdir(current_dir)

    return
