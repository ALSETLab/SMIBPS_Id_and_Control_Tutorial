from GridCal.Engine import *

def SMIB_gridcal(name = "SMIB", verbose = False):

    grid = MultiCircuit()

    # Grid name
    grid.name = "SMIB"

    # Changing the base of the system for the one used in Modelica
    grid.Sbase = 2220

    # Creating the buses of the system
    bus_1 = Bus('B1', vnom = 400)
    bus_2 = Bus('B2', vnom = 400)
    bus_3 = Bus('B3', vnom = 400)
    bus_3.is_slack = True # slack bus
    bus_4 = Bus('B4', vnom = 400)
    grid.add_bus(bus_1)
    grid.add_bus(bus_2)
    grid.add_bus(bus_3)
    grid.add_bus(bus_4)

    # Creating generators
    gen1 = Generator("gen_1", voltage_module = 1,
                    active_power = 0.899999999997135*grid.Sbase)
    slack = Generator("Slack ", voltage_module = 1)
    grid.add_generator(bus_1, gen1)
    grid.add_generator(bus_3, slack)

    # Creating lines
    line_1 = Branch(bus_2, bus_3, 'line 1', r = 0.0, x = 0.5, b = 0)
    line_2 = Branch(bus_2, bus_4, 'line_2', r = 0.0, x = 0.93/2, b = 0)
    line_3 = Branch(bus_4, bus_3, 'line_3', r = 0.0, x = 0.93/2, b = 0)
    grid.add_branch(line_1)
    grid.add_branch(line_2)
    grid.add_branch(line_3)

    # Creating transformer
    transformer = Branch(bus_from = bus_1,
                         bus_to = bus_2,
                         branch_type = BranchType.Transformer,
                         name = "transformer", 
                         x = 0.15,
                         r = 0)

    grid.add_branch(transformer)

    # Creating load
    load_ext_input = Load('load_ext_input', P = 100, Q = 200);
    grid.add_load(bus_4, load_ext_input)

    if verbose: # Show graph of the network
        grid.plot_graph()
        plt.show()
        
    return grid