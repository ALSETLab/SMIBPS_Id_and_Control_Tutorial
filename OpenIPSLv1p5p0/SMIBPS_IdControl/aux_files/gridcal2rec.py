import os
import pandas as pd
import numpy as np

def gridcal2rec(grid, pf, model_name, path_PF_data = None, pf_num = 1, export_pf_results = False):
    '''
    gridcal2rec

    DESCRIPTION:
    This function takes a GridCal multicircuit model and a power flow result and writes them into a power flow record compatible with Dymola. The template for the power flow is defined using the function 'create_pf_records'.

    ARGS:
    - grid: GridCal power flow model of the power system
    - pf: GridCal power flow object (containing power flow results)
    - model_name: name of the Modelica model (for creating records the first time)
    - path_PF_data: absolute path to the PF_data folder
    - pf_num: number of the power flow (for automation in an outer loop)
    - export_pf_results: export power flow results as '.csv' files: one for buses, another for machines

    '''

    # Getting the path to the 'PF_Data' folder (that should be the current working directory if nothing has been specified --for testing-)
    if path_PF_data is None:
        path_PF_data = os.getcwd()

    # Path to the 'PF_Data' subfolder
    pf_data_directory = os.path.join(path_PF_data, "PF_Data")

    # Creating the 'PF_Data' folder if it does not exist yet
    if not os.path.exists(pf_data_directory):
        os.makedirs(pf_data_directory)

    # Adding the 'PF_Data' reference to the 'package.order' file
    pk_order_path = os.path.join(path_PF_data, 'package.order') # Getting the path to the 'package.order' file

    # Checking if PF_Data already is in 'package.order'
    with open(pk_order_path, 'r') as file:
        lines = file.readlines()

    # If it is not, then append 'PF_Data' at the end of the package.order file
    with open(pk_order_path, 'a') as file:
        if 'PF_Data' not in lines:
            file.writelines('PF_Data')

    ####################################
    ### POWER FLOW RECORD CONTAINER ####
    ####################################

    # Power Flow record structure
    if 'PowerFlow_Data.mo' not in os.listdir(pf_data_directory):
        pf_data_path = os.path.join(pf_data_directory, 'PowerFlow_Data.mo')
        pf_data = open(pf_data_path, "w+")
        pf_data.write("within {}.PF_Data;\n".format(model_name))
        pf_data.write("record PowerFlow_Data\n")
        pf_data.write("extends Modelica.Icons.Record;\n")
        # Bus Voltage Records
        pf_data.write("replaceable record Bus = {}.PF_Data.Bus_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Bus_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Bus bus;\n\n")
        # Loads Records
        pf_data.write("replaceable record Loads = {}.PF_Data.Loads_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Loads_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Loads loads;\n\n")
        # Machine Records
        pf_data.write("replaceable record Machines = {}.PF_Data.Machines_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Machines_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Machines machines;\n\n")
        # Trafo Records
        pf_data.write("replaceable record Trafos = {}.PF_Data.Trafos_Template constrainedby\n".format(model_name))
        pf_data.write("PF_Data.Trafos_Template\n")
        pf_data.write("annotation(choicesAllMatching);\n")
        pf_data.write("Trafos trafos;\n\n")

        pf_data.write("end PowerFlow_Data;")
        pf_data.close()

    # Creating directories for power flow results (buses, loads, machines and trafos)

    bus_data_dir = os.path.join(pf_data_directory, "Bus_Data")
    loads_data_dir = os.path.join(pf_data_directory, "Loads_Data")
    machines_data_dir = os.path.join(pf_data_directory, "Machines_Data")
    trafos_data_dir = os.path.join(pf_data_directory, "Trafos_Data")

    ###############
    # BUS PACKAGE #
    ###############

    if not os.path.exists(bus_data_dir):
        os.makedirs(bus_data_dir)

    # Creating the 'package.mo' file for the bus data
    if 'package.mo' not in os.listdir(bus_data_dir):
        pack_mo_path = os.path.join(bus_data_dir, "package.mo")
        pack_mo = open(pack_mo_path, "w+")
        pack_mo.write("within {}.PF_Data;\n".format(model_name))
        pack_mo.write("package Bus_Data\n\n")
        pack_mo.write("end Bus_Data;")
        pack_mo.close()

    #################
    # LOADS PACKAGE #
    #################

    if not os.path.exists(loads_data_dir):
        os.makedirs(loads_data_dir)

    # Creating the 'package.mo' file for the bus data
    if 'package.mo' not in os.listdir(loads_data_dir):
        pack_mo_path = os.path.join(loads_data_dir, "package.mo")
        pack_mo = open(pack_mo_path, "w+")
        pack_mo.write("within {}.PF_Data;\n".format(model_name))
        pack_mo.write("package Loads_Data\n\n")
        pack_mo.write("end Loads_Data;")
        pack_mo.close()

    ####################
    # MACHINES PACKAGE #
    ####################

    if not os.path.exists(machines_data_dir):
        os.makedirs(machines_data_dir)

    # Creating the 'package.mo' file for the bus data
    if 'package.mo' not in os.listdir(machines_data_dir):
        pack_mo_path = os.path.join(machines_data_dir, "package.mo")
        pack_mo = open(pack_mo_path, "w+")
        pack_mo.write("within {}.PF_Data;\n".format(model_name))
        pack_mo.write("package Machines_Data\n\n")
        pack_mo.write("end Machines_Data;")
        pack_mo.close()

    ##################
    # TRAFOS PACKAGE #
    ##################

    if not os.path.exists(trafos_data_dir):
        os.makedirs(trafos_data_dir)

    # Creating the 'package.mo' file for the bus data
    if 'package.mo' not in os.listdir(trafos_data_dir):
        pack_mo_path = os.path.join(trafos_data_dir, "package.mo")
        pack_mo = open(pack_mo_path, "w+")
        pack_mo.write("within {}.PF_Data;\n".format(model_name))
        pack_mo.write("package Trafos_Data\n\n")
        pack_mo.write("end Trafos_Data;")
        pack_mo.close()

    #####################################
    ### WRITING LOAD POWER FLOW DATA ####
    #####################################

    # Creating '.mo' file for storing the load power flow results
    load_result_file_path = os.path.join(loads_data_dir, "PF_Loads_{}.mo".format(pf_num))
    load_result = open(load_result_file_path, "w+")
    load_result.write("record PF_Loads_{}\n".format(pf_num))
    load_result.write("extends {}.PF_Data.Loads_Template(\n\n".format(model_name))

    for n_load, load in enumerate(grid.get_loads()):

        load_result.write("// Load: '{}'\n".format(str(load.name)))
        load_result.write("PL{n_l} = {p_l},\n".format(n_l = n_load + 1, p_l = load.P*1000000))
        if n_load == len(grid.get_loads()) - 1:
            load_result.write("QL{n_l} = {q_l}\n\n".format(n_l = n_load + 1, q_l = load.Q*1000000))
        else:
            load_result.write("QL{n_l} = {q_l},\n\n".format(n_l = n_load + 1, q_l = load.Q*1000000))

    load_result.write(");\n")
    load_result.write("end PF_Loads_{};".format(pf_num))
    load_result.close()

    ####################################
    ### WRITING BUS POWER FLOW DATA ####
    ####################################

    bus_result_file_path = os.path.join(bus_data_dir, "PF_Bus_{}.mo".format(pf_num))
    bus_result = open(bus_result_file_path, "w+")
    bus_result.write("record PF_Bus_{}\n".format(pf_num))
    bus_result.write("extends {}.PF_Data.Bus_Template(\n\n".format(model_name))

    for n_bus, bus in enumerate(grid.buses):

        if pf.results.bus_types[n_bus] == 1:
            # PQ Bus
            bus_result.write("// Bus: '{}' (PQ bus)\n".format(bus.name))
        elif pf.results.bus_types[n_bus] == 2:
            # PV Bus
            bus_result.write("// Bus: '{}' (PV bus)\n".format(bus.name))
        elif pf.results.bus_types[n_bus] == 3:
            # Slack Bus
            bus_result.write("// Bus: '{}' (slack bus)\n".format(bus.name))

        # Writing bus voltage phasor magnitude
        bus_voltage_magnitude = np.abs(pf.results.voltage[n_bus])
        bus_result.write("V{b_name} = {v_magn:.7f},\n".format(b_name = n_bus + 1, v_magn = bus_voltage_magnitude))

        # Writing bus voltage phasor angle in degrees
        bus_voltage_angle = np.arctan(np.imag(pf.results.voltage[n_bus])/np.real(pf.results.voltage[n_bus]))
        if n_bus == len(grid.buses) - 1:
            bus_result.write("A{b_name} = {v_angl:.7f}\n\n".format(b_name = n_bus + 1, v_angl = bus_voltage_angle))
        else:
            bus_result.write("A{b_name} = {v_angl:.7f},\n\n".format(b_name = n_bus + 1, v_angl = bus_voltage_angle))

    bus_result.write(");\n")
    bus_result.write("end PF_Bus_{};".format(pf_num))
    bus_result.close()

    ############################################
    ### WRITING TRANSFORMER POWER FLOW DATA ####
    ############################################

    # Containers for information about the buses
    br_trafos_index = []
    br_trafos_from = []
    br_trafos_name = []
    br_trafos_to = []

    # Getting indices of the branches which are a transformer in the grid object
    for n_branch, branch in enumerate(grid.get_branches()):
        if str(branch.branch_type) == 'transformer':
            br_trafos_index.append(n_branch)
            br_trafos_from.append(branch.bus_from)
            br_trafos_to.append(branch.bus_to)
            br_trafos_name.append(branch.name.split()) # Strip is used to remove blank spaces from the name

    trafos_result_file_path = os.path.join(trafos_data_dir, "PF_Trafos_{}.mo".format(pf_num))
    trafos_result = open(trafos_result_file_path, "w+")
    trafos_result.write("record PF_Trafos_{}\n".format(pf_num))
    trafos_result.write("extends {}.PF_Data.Trafos_Template(\n\n".format(model_name))

    for n_trafo, trafo in enumerate(pf.results.tap_module):
        trafos_result.write("// TRAFO: '{}'\n".format(br_trafos_name[n_trafo]))
        trafos_result.write("// From: '{}' - To: '{}'\n".format(br_trafos_from[n_trafo], br_trafos_to[n_trafo]))
        trafos_result.write("t1_trafo_{n_traf} = {t:.7f},\n".format(n_traf = n_trafo + 1, t = pf.results.tap_module[n_trafo]))
        if n_trafo == len(br_trafos_index) - 1:
            trafos_result.write("t2_trafo_{n_traf} = 1.0000000\n\n".format(n_traf = n_trafo + 1))
        else:
            trafos_result.write("t2_trafo_{n_traf} = 1.0000000,\n\n".format(n_traf = n_trafo + 1))

    trafos_result.write(");\n")
    trafos_result.write("end PF_Trafos_{};".format(pf_num))
    trafos_result.close()

    ############################################
    ### WRITING GENERATOR POWER FLOW DATA ######
    ############################################

    # Creating the '.mo' file
    machines_result_file_path = os.path.join(machines_data_dir, "PF_Machines_{}.mo".format(pf_num))
    machines_result = open(machines_result_file_path, "w+")
    machines_result.write("record PF_Machines_{}\n".format(pf_num))
    machines_result.write("extends {}.PF_Data.Machines_Template(\n\n".format(model_name))

    # Container for bus names
    bus_names = []
    for bus in grid.get_buses():
        bus_names.append(str(bus.name).strip())

    # Creating Pandas DataFrame with power flow results and bus type for each bus (power only)
    pf_S_bus_results = pd.DataFrame(columns = [], index = bus_names)

    # Adding active power to the buses
    pf_S_bus_results["P [MW]"] = np.real(pf.results.Sbus*grid.Sbase)

    # Adding reactive power to the buses
    pf_S_bus_results["Q [MVAR]"] = np.imag(pf.results.Sbus*grid.Sbase)

    # Adding the bus type
    pf_S_bus_results["Bus_Type"] = pf.results.bus_types

    # Getting information of the generators and loads of the system
    gen_name = []
    gen_bus = []
    gen_P = []

    load_name = []
    load_bus = []
    load_Q = []
    load_P = []

    for gen in grid.get_generators():
        gen_name.append(str(gen.name).strip())
        gen_bus.append(str(gen.bus).strip())
        gen_P.append(gen.P)

    for load in grid.get_loads():
        load_name.append(str(load.name).strip())
        load_bus.append(str(load.bus).strip())
        load_P.append(load.P)
        load_Q.append(load.Q)

    df_gen_data = pd.DataFrame(columns = [], index = gen_name)
    df_gen_data["Bus"] = gen_bus
    df_gen_data["P [MW]"] = gen_P

    df_load_data = pd.DataFrame(columns = [], index = load_name)
    df_load_data["P [MW]"] = load_P
    df_load_data["Bus"] = load_bus
    df_load_data["Q [MVAR]"] = load_Q

    gen_P = []
    gen_Q = []

    for n_gen, gen in enumerate(gen_name):

        # Getting the bus to which the generator is connected
        gen_bus = df_gen_data.loc[gen]["Bus"]

        # Getting the bus type
        bus_type = pf_S_bus_results.loc[gen_bus]["Bus_Type"]

        if bus_type == 3:
            # If the bus is the slack bus, then read the generated power from the power flow

            # Net active power at the corresponding bus
            P_net_bus = pf_S_bus_results.loc[gen_bus]["P [MW]"]

            # Sum of all reactive power of the loads connected to the generation bus (if any)
            P_load_sum = np.abs(df_load_data.loc[df_load_data["Bus"] == gen_bus]["P [MW]"].sum())

            # Generated reactive power at the bus
            P_gen_bus = P_net_bus + P_load_sum

            # Number of generators connected at the bus
            n_gens = len(df_gen_data.loc[df_gen_data["Bus"] == gen_bus])

            if n_gens == 1:
                P_machine = P_gen_bus
                gen_P.append(P_machine)
            else:
                # Distribute 'P' along all machines equally (for the time being)
                P_machine = P_gen_bus / n_gens
                gen_P.append(P_machine)

        else:
            # Getting the active power dispatch of the generator
            P_machine = df_gen_data.loc[gen]["P [MW]"]
            gen_P.append(P_machine)

        # Net reactive power at the corresponding bus
        Q_net_bus = pf_S_bus_results.loc[gen_bus]["Q [MVAR]"]

        # Sum of all reactive power of the loads connected to the generation bus (if any)
        Q_load_sum = np.abs(df_load_data.loc[df_load_data["Bus"] == gen_bus]["Q [MVAR]"].sum())

        # Generated reactive power at the bus
        Q_gen_bus = Q_net_bus + Q_load_sum

        # Number of generators connected at the bus
        n_gens = len(df_gen_data.loc[df_gen_data["Bus"] == gen_bus])

        if n_gens == 1:
            Q_machine = Q_gen_bus
            gen_Q.append(Q_machine)
        else:
            # Distribute 'Q' along all machines equally (for the time being)
            Q_machine = Q_gen_bus / n_gens
            gen_Q.append(Q_machine)

        machines_result.write("// MACHINE: '{}'\n".format(gen))
        machines_result.write("// Bus: {}'\n".format(gen_bus))
        machines_result.write("PG{} = {:.7f},\n".format(n_gen + 1, P_machine*1000000))

        if n_gen == len(gen_name) - 1:
            machines_result.write("QG{} = {:.7f}\n\n".format(n_gen + 1, Q_machine*1000000))
        else:
            machines_result.write("QG{} = {:.7f},\n\n".format(n_gen + 1, Q_machine*1000000))

    machines_result.write(");\n")
    machines_result.write("end PF_Machines_{};".format(pf_num))
    machines_result.close()

    if export_pf_results:

        # EXPORTING PF RESULTS AS '.csv' FILES

        # Creating the csv file directory
        pf_data_csv_dir = os.path.join(pf_data_directory, "PF_Results_csv")

        if not os.path.exists(pf_data_csv_dir):
            os.makedirs(pf_data_csv_dir)

        ######################
        #### BUS VOLTAGE #####
        ######################

        headers = ["Bus Type", "|V| [pu]", "d [deg]"]
        df_voltage = pd.DataFrame(index = bus_names, columns = headers)
        df_voltage["Bus Types"] = pf.results.bus_types
        df_voltage["|V| [pu]"] = np.abs(pf.results.voltage)
        df_voltage["d [deg]"] = np.arctan(np.imag(pf.results.voltage)/np.real(pf.results.voltage))*180/np.pi

        # Exporting to '.csv'
        voltage_results = os.path.join(pf_data_csv_dir, f"PF_bus_results_{pf_num}.csv")
        df_voltage.to_csv(voltage_results, index = True)

        ########################
        #### MACHINE POWER #####
        ########################

        headers = ["P [MW]", "Q [MVAR]"]
        df_machines = pd.DataFrame(index = gen_name, columns = headers)
        df_machines["P [MW]"] = gen_P
        df_machines["Q [MVAR]"] = gen_Q

        # Exporting to '.csv'
        machine_results = os.path.join(pf_data_csv_dir, f"PF_machine_results_{pf_num}.csv")
        df_machines.to_csv(machine_results, index = True)

        ########################
        ######## LOADS #########
        ########################

        # Exporting to '.csv'
        load_results = os.path.join(pf_data_csv_dir, f"PF_load_results_{pf_num}.csv")
        df_load_data.to_csv(load_results, index = True)

        ####################################
        ######## NET POWER PER BUS #########
        ####################################

        # Per bus
        headers = ["P [MW]", "Q [MVAR]", "Bus_Type"]
        df_bus = pd.DataFrame(index = bus_names, columns = headers)

        # Bus results data frame
        df_bus["P [MW]"] = np.real(pf.results.Sbus)*grid.Sbase
        df_bus["Q [MVAR]"] = np.imag(pf.results.Sbus)*grid.Sbase
        df_bus["Bus_Type"] = pf.results.bus_types

        # Exporting to '.csv'
        bus_power_results = os.path.join(pf_data_csv_dir, f"PF_bus_power_results_{pf_num}.csv")
        df_bus.to_csv(bus_power_results, index = True)
