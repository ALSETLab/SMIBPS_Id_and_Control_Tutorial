import re
import os

def remove_prefix(text, prefix):
    '''
    Description:

    Function to remove a given prefix from a list

    Arguments:
    - text: given text container (preferably a string)
    - prefix: prefix that is to be removed from 'text'
    '''

    if text.startswith(prefix):
        return text[len(prefix):]
    return text  # or whatever

def generate_component_list(model_name):
    '''

    DESCRIPTION: this function generates a dictionary of lists with the component names of the devices employed to construct the power system models using the OpenIPSL library.

    ----------------------
    INPUTS:
    ----------------------
    - model_name: name of the model (i.e., IEEE_9 for a IEEE 9 bus model, etc)

    ----------------------
    OUTPUTS:
    ----------------------
    - components: dictionary containing lists of the line, transformer, generator, load, bus and faults model name in each of the models. Note that the keys of the dictionaries are 'lines', 'trafos', 'generators', 'loads', 'buses' and 'faults'.
    '''

    lines = []
    trafos = []
    generators = []
    loads = []
    buses = []
    faults = []

    #############################################
    #     READING COMPONENT NAME FROM MODELS    #
    #############################################

    # Opening raw text file of the '.mo' file
    model_mo = open(model_name, "r")
    # Parsing file into a list and splitting it per newline. The output is a list
    model_mo_text = model_mo.read().split('\n')

    # Converting the list into a single string
    model_mo_full_text = '\n'.join(model_mo_text)

    # Regular expression for finding lines
    match_line = re.finditer(r"OpenIPSL.Electrical.Branches.PwLine (\w)*", model_mo_full_text)

    # Regular expression for finding transformers (substations) connecting buses
    match_trafo = re.finditer(r"OpenIPSL.Electrical.Branches.(PSSE|PSAT).TwoWindingTransformer[\r\n]+([^\r\n]+)|OpenIPSL.Electrical.Branches.(PSSE|PSAT).TwoWindingTransformer (\w)*|OpenIPSL.Electrical.Branches.(PSSE|PSAT).TWTransformerWithFixedTapRatio (\w)*|OpenIPSL.Electrical.Branches.(PSSE|PSAT).TWTransformerWithFixedTapRatio[\r\n]+([^\r\n]+)", model_mo_full_text)

    # Regular expression for finding generators
    match_gen = re.finditer(r"[\w.]*(\w)*Generat(\w)*.(\w)* (\w)*|Generat[\r\n]*", model_mo_full_text)

    # Regular expression for finding buses
    match_bus = re.finditer(r"OpenIPSL.Electrical.Buses.BusExt (\w)*", model_mo_full_text)

    # Regular expression for finding loads
    match_loads = re.finditer(r"OpenIPSL.Electrical.Loads.(PSAT|PSSE).(\w)* (\w)*", model_mo_full_text)

    # Regular expression for finding faults
    match_faults = re.finditer(r"OpenIPSL.Electrical.Events.PwFault (\w)*", model_mo_full_text)

    for m in match_gen:
        # Removing class declaration
        generator_name = m.group().split(" ")[1]
        generators.append(generator_name)

    for m in match_loads:
        load_name = m.group().split(" ")[1]
        loads.append(load_name)

    for m in match_line:
        # Removing modifier to get the names of the lines
        lines.append(remove_prefix(m.group(), 'OpenIPSL.Electrical.Branches.PwLine '))

    for m in match_bus:
        bus_name = m.group().split(" ")[1]
        buses.append(bus_name)

    for m in match_faults:
        fault_name = m.group().split(" ")[1]
        faults.append(fault_name)

    for m in match_trafo:

        # Removing modifiers to get the names of the transformers
        if '\n' in m.group():
            # Declaration of the transformer in two lines
            match_name = re.findall(r'[^\s\(]', m.group().split('\n')[1])
        else:
            # Declaration of the transformer in one line

            # Getting declaration statement (see if this is only required for the one line declaration)
            declaration_string = m.group().split(" ")[0]
            match_name = re.findall(r'[^\s\(]', remove_prefix(m.group(), declaration_string))

        # Name of the transformer
        trafo_name = ''.join(match_name)
        if trafo_name == "":
            print(m.group())

        # Adding transformer name
        trafos.append(trafo_name)

    components = {'lines': lines, 'trafos' : trafos, 'generators' : generators, 'loads' : loads, 'buses' : buses, 'faults' : faults}

    return components
