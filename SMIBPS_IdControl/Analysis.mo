within SMIBPS_IdControl;
package Analysis
  extends Modelica.Icons.ExamplesPackage;

  package Simulation
    extends Modelica.Icons.ExamplesPackage;
    model SMIB
      extends Modelica.Icons.Example;
      extends BaseModelsPartial.BaseNetwork.SMIB_Partial;
      import Modelica.Constants.pi;
      BaseModelsPartial.BasePlants.Generator G1(
        V_0=1,
        P_0=0.899999999997135*S_b,
        Q_0=0.436002238696658*S_b,
        angle_0=0.494677176989155*180/pi)
        annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(G1.pwPin, B1.p)
        annotation (Line(points={{-99,0},{-80,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-110,68},{110,48}},
              lineColor={0,0,0},
              lineThickness=1,
              fillPattern=FillPattern.Solid,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="(Constant Efd)")}),
        Icon(coordinateSystem(extent={{-140,-100},{120,100}})),
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-006,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Rkfix2"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
    end SMIB;

    model SMIB_AVR
        extends Modelica.Icons.Example;
      extends BaseModelsPartial.BaseNetwork.SMIB_Partial;
      import Modelica.Constants.pi;
      BaseModelsPartial.BasePlants.Generator_AVR G1(
        V_0=1,
        P_0=0.899999999997135*S_b,
        Q_0=0.436002238696658*S_b,
        angle_0=0.494677176989155*180/pi)
        annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(G1.pwPin, B1.p)
        annotation (Line(points={{-99,0},{-90,0},{-80,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-110,70},{110,50}},
              lineColor={0,0,0},
              lineThickness=1,
              fillPattern=FillPattern.Solid,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="(AVR)")}),
        Icon(coordinateSystem(extent={{-140,-100},{120,100}})),
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-006,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Rkfix2"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
    end SMIB_AVR;

    model SMIB_AVR_PSS
        extends Modelica.Icons.Example;
      extends BaseModelsPartial.BaseNetwork.SMIB_Partial;
      import Modelica.Constants.pi;
      BaseModelsPartial.BasePlants.Generator_AVR_PSS G1(
        V_0=1,
        P_0=0.899999999997135*S_b,
        Q_0=0.436002238696658*S_b,
        angle_0=0.494677176989155*180/pi)
        annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(G1.pwPin, B1.p)
        annotation (Line(points={{-99,0},{-90,0},{-80,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-110,74},{110,54}},
              lineColor={0,0,0},
              lineThickness=1,
              fillPattern=FillPattern.Solid,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="(AVR + PSS)")}),
        Icon(coordinateSystem(extent={{-140,-100},{120,100}})),
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-006,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Rkfix2"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
    end SMIB_AVR_PSS;
  end Simulation;

  package LinearAnalysis
    extends Modelica.Icons.ExamplesPackage;
    package Interfaces
      model SMIB_GEN_wInput
        extends BaseModelsPartial.BaseNetwork.SMIB_Base;
          extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;

        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_wInputs G1(
          V_0=1,
          P_0=0.899999999997135*S_b,
          Q_0=0.436002238696658*S_b,
          angle_0=0.494677176989155*180/pi)
          annotation (Placement(transformation(extent={{-124,-12},{-100,12}})));
        Modelica.Blocks.Interfaces.RealInput uEfd
          annotation (Placement(transformation(extent={{-180,20},{-140,60}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-60},{-140,-20}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-98.8,0},{-80,0}},       color={0,0,255}));
        connect(G1.pm, uPm) annotation (Line(points={{-126.4,-7.2},{-132,-7.2},{-132,-40},
                {-160,-40}}, color={0,0,127}));
        connect(G1.efd, uEfd) annotation (Line(points={{-126.4,7.2},{-134,7.2},{-134,40},
                {-160,40}}, color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}),
                                            graphics={Text(
                extent={{-108,140},{112,120}},
                lineColor={0,0,0},
                lineThickness=1,
                fontSize=15,
                textStyle={TextStyle.Bold},
                textString="(Generator + input for Bode plots)"),
                                                            Text(
                extent={{-114,-58},{2,-80}},
                lineColor={28,108,200},
                textString="Output value set is B1.V.
Output variable can be redifined in the text layer.",
                horizontalAlignment=TextAlignment.Left)}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
Generator
"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_GEN_wInput;
      extends Modelica.Icons.InterfacesPackage;
      model SMIB_AVR_wInput
        extends BaseModelsPartial.BaseNetwork.SMIB_Base;
          extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;

        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_wInputs G1(
          V_0=1,
          P_0=0.899999999997135*S_b,
          Q_0=0.436002238696658*S_b,
          angle_0=0.494677176989155*180/pi)
          annotation (Placement(transformation(extent={{-124,-12},{-100,12}})));
        Modelica.Blocks.Interfaces.RealInput uVref
          annotation (Placement(transformation(extent={{-180,20},{-140,60}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-60},{-140,-20}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-98.8,0},{-80,0}},       color={0,0,255}));
        connect(uVref, G1.uVs) annotation (Line(points={{-160,40},{-134,40},{-134,7.2},
                {-126.64,7.2}}, color={0,0,127}));
        connect(G1.pm, uPm) annotation (Line(points={{-126.4,-7.2},{-132,-7.2},{-132,-40},
                {-160,-40}}, color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}),
                                            graphics={Text(
                extent={{-138,138},{138,118}},
                lineColor={0,0,0},
                lineThickness=1,
                fontSize=15,
                textStyle={TextStyle.Bold},
                textString="(AVR
 + input for Bode plots)"),                                 Text(
                extent={{-114,-58},{2,-80}},
                lineColor={28,108,200},
                textString="Output value set is B1.V.
Output variable can be redifined in the text layer.",
                horizontalAlignment=TextAlignment.Left)}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
with AVR
"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_AVR_wInput;

      model SMIB_AVR_PSS_wInput
        extends BaseModelsPartial.BaseNetwork.SMIB_Base;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        import Modelica.Constants.pi;
        BaseModelsPartial.BasePlants.Generator_AVR_PSS_wInputs G1(
          V_0=1,
          P_0=0.899999999997135*S_b,
          Q_0=0.436002238696658*S_b,
          angle_0=0.494677176989155*180/pi)
          annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
        Modelica.Blocks.Interfaces.RealInput uPSS
          annotation (Placement(transformation(extent={{-180,20},{-140,60}})));
        Modelica.Blocks.Interfaces.RealInput uPm
          annotation (Placement(transformation(extent={{-180,-60},{-140,-20}})));
      protected
        parameter Real S_b=SysData.S_b;
      equation
        w = G1.machine.w;
        delta = G1.machine.delta;
        Vt = G1.machine.v;
        P = G1.machine.P;
        Q = G1.machine.Q;
        connect(G1.pwPin, B1.p)
          annotation (Line(points={{-99,0},{-80,0}},         color={0,0,255}));
        connect(G1.uPSS, uPSS) annotation (Line(points={{-122,6},{-128,6},{-128,40},{-160,
                40}}, color={0,0,127}));
        connect(uPm, G1.pm) annotation (Line(points={{-160,-40},{-130,-40},{-130,-6},{
                -122,-6}}, color={0,0,127}));
        annotation (
          Diagram(coordinateSystem(extent={{-140,-140},{140,140}}),
                                            graphics={Text(
                extent={{-118,140},{102,120}},
                lineColor={0,0,0},
                lineThickness=1,
                fontSize=15,
                textStyle={TextStyle.Bold},
                textString="(AVR + PSS + input for Bode plots)"), Text(
                extent={{-114,-58},{2,-80}},
                lineColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Output value set is G1.w, the generator speed.
Output variable can be redifined in the text layer.")}),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}}), graphics={
              Rectangle(extent={{-140,140},{140,-140}}, lineColor={28,108,200}),
              Text(
                extent={{-140,60},{140,-80}},
                lineColor={28,108,200},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="Nonlinear
Model
AVR+PSS

"),           Text(
                extent={{-140,-80},{140,-220}},
                lineColor={238,46,47},
                fillColor={255,255,255},
                fillPattern=FillPattern.Solid,
                textString="%name
")}),     experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"));
      end SMIB_AVR_PSS_wInput;

      partial model OutputsInterface
        Modelica.Blocks.Interfaces.RealOutput Vt
          annotation (Placement(transformation(extent={{140,68},{160,90}})));
      public
        Modelica.Blocks.Interfaces.RealOutput Q
          annotation (Placement(transformation(extent={{140,-10},{160,12}})));
        Modelica.Blocks.Interfaces.RealOutput P
          annotation (Placement(transformation(extent={{140,30},{160,52}})));
        Modelica.Blocks.Interfaces.RealOutput w
          annotation (Placement(transformation(extent={{140,-50},{160,-28}})));
        Modelica.Blocks.Interfaces.RealOutput delta
          annotation (Placement(transformation(extent={{140,-92},{160,-70}})));
        annotation (
          experiment(
            StopTime=10,
            Interval=0.0001,
            Tolerance=1e-06,
            __Dymola_fixedstepsize=0.0001,
            __Dymola_Algorithm="Dassl"),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<table cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr>
<td><p>Reference</p></td>
<td><p>SMIB PSAT, d_kundur2.mdl, PSAT</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td>February 2016</td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>Maxime Baudette, Ahsan Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"),Diagram(coordinateSystem(extent={{-140,-140},{140,140}})),
          Icon(coordinateSystem(extent={{-140,-140},{140,140}})));
      end OutputsInterface;

    end Interfaces;

    package Linearization
      "Gives an example of basic linearization analysis for the SMIB."
      extends Modelica.Icons.ExamplesPackage;
      function LinearizeSMIB
        // Import things needed for the calculations
        import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
        // Declare outputs to display
        output Real A[:,:] "A-matrix";
        output Real B[:,:] "B-matrix";
        output Real C[:,:] "C-matrix";
        output Real D[:,:] "D-matrix";
        output String inputNames[:] "Modelica names of inputs";
        output String outputNames[:] "Modelica names of outputs";
        output String stateNames[:] "Modelica names of states";
        // Declare reconfigurable inputs
        input Modelica.SIunits.Time tlin = 0 "t for model linearization";
        input Modelica.SIunits.Time tsim = 15 "Simulation time";
        input String pathToNonlinearPlantModel = "SMIB_PSControl.Analysis.LinearAnalysis.Interfaces.SMIB_GEN_wInput";
        input String pathToNonlinearExperiment=
            "SMIB_PSControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbGen";
        input String pathToLinearExperiment = "SMIB_PSControl.Analysis.LinearAnalysis.BasicLinearization.LinearModelExample";

      algorithm
        // Compute and display the ABCD matrices, etc
        Modelica.Utilities.Streams.print("Linearized Model");
        (A,B,C,D,inputNames,outputNames,stateNames) :=
          Modelica_LinearSystems2.Utilities.Import.linearize(
          pathToNonlinearPlantModel,tlin);
        nx := size(A, 1); //number of states
        Modelica.Utilities.Streams.print("Number of states" + String(nx));
        // Now we want to extract the initial value of the outputs to use it in our
        // linear model response
        Modelica.Utilities.Streams.print("Simulating nonlinear model");
        simulateModel(
          pathToNonlinearExperiment,
          stopTime=tsim,
          numberOfIntervals=1000,
          resultFile="res_nl");
         ny := size(C, 1);
         y0 := DymolaCommands.Trajectories.readTrajectory(
           "res_nl.mat",
           {outputNames[i] for i in 1:ny},
           DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
         DataFiles.writeMATmatrix(
           "MyData.mat",
           "y0",
           [y0[1:ny,1]],
           append=true);
        // Print y0's first values which is needed for the linear response model
        y0out := y0[:,1]; // we only want the first few elements
        Modelica.Utilities.Streams.print("y0 =");
        Modelica.Math.Vectors.toString(y0out);
        annotation(__Dymola_interactive=true);
      end LinearizeSMIB;

      model LinearModelExample
        "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepEfd(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-120,22},{-100,42}})));
        inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
          annotation (Placement(transformation(extent={{72,70},{92,90}})));
        Modelica.Blocks.Routing.Multiplex2 multiplex2_1(n1=1, n2=1)
          annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
        Modelica_LinearSystems2.Controller.StateSpace stateSpace(system(
            A=[0.0,376.99111838914104,0.0,0.0,0.0,0.0; -0.15935012502522555,
                0.0,0.0,0.0,-0.1730203086974122,0.06000759113665713; -0.225786591276862,
                0.0,-0.12499999999582975,0.0,-0.2668898899908987,-0.0011041056183099536;
                0.46107962381245693,0.0,0.0,-1.0000000000758573,
                0.006336424569356028,-1.4894289460374501; -2.9728307608838804,
                0.0,33.33333333222127,0.0,-36.84735279230479,-0.014537265192078836;
                2.6215590291419555,0.0,0.0,14.285714286797962,
                0.0360269902464371,-22.754155959037135],
            B=[0.0,0.0; 0.0,0.14285715467719587; 0.12500001034254637,0.0; 0.0,
                0.0; 0.0,0.0; 0.0,0.0],
            C=[-0.13790996444876555,0.0,0.0,0.0,0.5004542799003135,
                0.438830511745594; 0.314162109372547,0.0,0.0,0.0,
                1.6774953338447993,0.6769042375621069; 1.107819258076765,0.0,
                0.0,0.0,1.2032592305120056,-0.4169392158037044; 0.0,
                0.999999999889562,0.0,0.0,0.0,0.0; 0.9999999999701366,0.0,0.0,
                0.0,0.0,0.0],
            D=[0.0,0.0; 0.0,0.0; 0.0,0.0; 0.0,0.0; 0.0,0.0],
            yNames={"Vt","Q","P","w","delta"},
            xNames={"G1.machine.delta","G1.machine.w","G1.machine.e1q",
                "G1.machine.e1d","G1.machine.e2q","G1.machine.e2d"},
            uNames={"uEfd","uPm"}), blockType=Modelica_LinearSystems2.Controller.Types.BlockTypeWithGlobalDefault.Continuous)
          annotation (Placement(transformation(extent={{-36,-10},{-16,10}})));
        Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
          annotation (Placement(transformation(extent={{60,-20},{100,20}})));
        Modelica.Blocks.Math.Add addy[5]
          annotation (Placement(transformation(extent={{6,-16},{26,4}})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0)
          annotation (Placement(transformation(extent={{-120,-40},{-100,-20}})));
        Modelica.Blocks.Routing.Multiplex5 multiplex5_1 annotation (Placement(
              transformation(
              extent={{-14,-14},{14,14}},
              rotation=90,
              origin={2,-38})));
        Modelica.Blocks.Sources.Constant y0_Vt(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-80,-90})));
        Modelica.Blocks.Sources.Constant y0_Q(k=0.4360022246837616)
                                                          annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-40,-90})));
        Modelica.Blocks.Sources.Constant y0_P(k=0.8999999761581421)
                                                     annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={0,-90})));
        Modelica.Blocks.Sources.Constant y0_w(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={40,-90})));
        Modelica.Blocks.Sources.Constant y0_delta(k=1.2242474555969238)
                                                             annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={80,-90})));
      equation
        connect(multiplex2_1.u1[1], stepEfd.y) annotation (Line(points={{-82,6},{-90,6},
                {-90,32},{-99,32}}, color={0,0,127}));
        connect(multiplex2_1.y, stateSpace.u)
          annotation (Line(points={{-59,0},{-38,0}}, color={0,0,127}));
        connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{108,16},{
                108,79},{150,79}},
                               color={0,0,127}));
        connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{110,-8},{110,
                -39},{150,-39}}, color={0,0,127}));
        connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},{108,-16},
                {108,-81},{150,-81}}, color={0,0,127}));
        connect(addy.y, demultiplex2_2.u)
          annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
        connect(stateSpace.y, addy.u1)
          annotation (Line(points={{-15,0},{4,0}}, color={0,0,127}));
        connect(Pmchange.y, multiplex2_1.u2[1]) annotation (Line(points={{-99,-30},{-90,
                -30},{-90,-6},{-82,-6}}, color={0,0,127}));
        connect(multiplex5_1.y, addy.u2)
          annotation (Line(points={{2,-22.6},{2,-12},{4,-12}}, color={0,0,127}));
        connect(multiplex5_1.u1[1], y0_Vt.y) annotation (Line(points={{-12,-54.8},{-80,
                -54.8},{-80,-79}}, color={0,0,127}));
        connect(y0_Q.y, multiplex5_1.u2[1]) annotation (Line(points={{-40,-79},{-40,-64},
                {-5,-64},{-5,-54.8}}, color={0,0,127}));
        connect(y0_P.y, multiplex5_1.u3[1])
          annotation (Line(points={{0,-79},{2,-79},{2,-54.8}}, color={0,0,127}));
        connect(y0_w.y, multiplex5_1.u4[1]) annotation (Line(points={{40,-79},{40,-64},
                {9,-64},{9,-54.8}}, color={0,0,127}));
        connect(y0_delta.y, multiplex5_1.u5[1]) annotation (Line(points={{80,-79},{80,
                -54.8},{16,-54.8}}, color={0,0,127}));
        connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{
                122,0},{122,41},{150,41}}, color={0,0,127}));
        connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,1},{
                126,1},{126,8},{102,8}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
              Text(
                extent={{-58,24},{-44,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="du",
                fontSize=24),
              Text(
                extent={{-38,-18},{-22,-38}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="y0",
                fontSize=24),
              Text(
                extent={{40,20},{50,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                fontSize=24,
                textString="y"),
              Text(
                extent={{-12,22},{2,6}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                fontSize=24,
                textString="dy"),
              Text(
                extent={{-80,80},{0,60}},
                lineColor={85,170,255},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
              Line(
                points={{-4,64},{24,44},{18,14}},
                color={0,0,255},
                thickness=0.5,
                arrow={Arrow.None,Arrow.Filled},
                smooth=Smooth.Bezier)}),
          experiment(
            StopTime=15,
            __Dymola_NumberOfIntervals=10000,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Rkfix4"));
      end LinearModelExample;

      function LinearizeSMIBGeneral
        "Makes the approach shown in the LinearModelExample and LinearizeSMIB more general."
        // Import things needed for the calculations
        import Modelica_LinearSystems2.StateSpace; // to create and manipulate state space objects
        // Declare outputs to display
        output Real A[:,:] "A-matrix";
        output Real B[:,:] "B-matrix";
        output Real C[:,:] "C-matrix";
        output Real D[:,:] "D-matrix";
        output String inputNames[:] "Modelica names of inputs";
        output String outputNames[:] "Modelica names of outputs";
        output String stateNames[:] "Modelica names of states";
        output Real y0out[:] "Initial value of the output variables";
        // Declare reconfigurable inputs
        input Modelica.SIunits.Time tlin = 0 "t for model linearization";
        input Modelica.SIunits.Time tsim = 15 "Simulation time";
        input String pathToNonlinearPlantModel = "SMIB_PSControl.Analysis.LinearAnalysis.Interfaces.SMIB_GEN_wInput";
        input String pathToNonlinearExperiment=
            "SMIB_PSControl.Analysis.LinearAnalysis.PerturbationAnalysis.PerturbGen";
        input String pathToLinearExperiment = "SMIB_PSControl.Analysis.LinearAnalysis.Linearization.LinearModelGeneral";

      algorithm
        // Compute and display the ABCD matrices, etc
        (A,B,C,D,inputNames,outputNames,stateNames) :=
          Modelica_LinearSystems2.Utilities.Import.linearize(
          pathToNonlinearPlantModel,tlin);
        // LINEARIZE plant model at t_lin
        // This is the same as above, however, it stores it in a StateSpace object
        ss := Modelica_LinearSystems2.ModelAnalysis.Linearize(
          pathToNonlinearPlantModel, simulationSetup=
          Modelica_LinearSystems2.Records.SimulationOptionsForLinearization(
          linearizeAtInitial=false, t_linearize=tlin));
        // PRINT linear system
        Modelica.Utilities.Streams.print(String(ss));
        // SAVE the data in a mat file
        DataFiles.writeMATmatrix(
          "MyData.mat",
          "ABCD",
          [ss.A, ss.B; ss.C, ss.D],
          append=false);
        nx := size(ss.A, 1);
        DataFiles.writeMATmatrix(
          "MyData.mat",
          "nx",
          [nx],
          append=true);
        Modelica.Utilities.Streams.print("Simulating nonlinear model");
        simulateModel(
          pathToNonlinearExperiment,
          stopTime=tsim,
          numberOfIntervals=10000, method = "Rkfix4", fixedstepsize=0.01,
          resultFile="res_nl");
         ny := size(ss.C, 1);
         y0 := DymolaCommands.Trajectories.readTrajectory(
           "res_nl.mat",
           {ss.yNames[i] for i in 1:ny},
           DymolaCommands.Trajectories.readTrajectorySize("res_nl.mat"));
          // {"PS_ConstantEfd." + ss.yNames[i] for i in 1:ny},
         DataFiles.writeMATmatrix(
           "MyData.mat",
           "y0",
           [y0[1:ny,1]],
           append=true);
        // Print y0's first values which is needed for the linear response model
        y0out := y0[:,1]; // we only want the first few elements
        Modelica.Utilities.Streams.print("y0 =");
        Modelica.Math.Vectors.toString(y0out);
        //
        // We now simulate the linear model, which requires y0
        Modelica.Utilities.Streams.print("Simulating linear model");
        simulateModel(
          pathToLinearExperiment,
          stopTime=tsim,
          numberOfIntervals=10000, method = "Rkfix4", fixedstepsize=0.01,
          resultFile="res_lin");
        annotation(__Dymola_interactive=true);
      end LinearizeSMIBGeneral;

      model LinearModelGeneral
        "Simulate the linearized SMIB model obtained by running the function LinearizeSMIB."
        extends SMIBPS_IdControl.Utilities.Icons.FunctionDependentExample;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        // The following definitions are very important to couple the linear model
        // to the linearization of the nonlinear model and the simulation
        parameter Real[:] y0=vector(DataFiles.readMATmatrix("MyData.mat", "y0")) annotation (Evaluate=false);
        // The following has to be imported in order to be able to interpret and manipulate the StateSpace types
        import Modelica_LinearSystems2.StateSpace;
        parameter StateSpace ss=StateSpace.Import.fromFile("MyData.mat", "ABCD");
        parameter Integer ny=size(ss.C, 1);
        Modelica.Blocks.Sources.Step stepEfd(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-120,22},{-100,42}})));
        inner Modelica_LinearSystems2.Controller.SampleClock sampleClock
          annotation (Placement(transformation(extent={{60,60},{80,80}})));
        Modelica.Blocks.Routing.Multiplex2 multiplex2_1(n1=1, n2=1)
          annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
        Modelica.Blocks.Routing.DeMultiplex5 demultiplex2_2
          annotation (Placement(transformation(extent={{60,-20},{100,20}})));
        Modelica.Blocks.Math.Add addy[ny]
          annotation (Placement(transformation(extent={{6,-16},{26,4}})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0)
          annotation (Placement(transformation(extent={{-120,-40},{-100,-20}})));
        Modelica.Blocks.Sources.Constant y0_initial[ny](k=y0)      annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={0,-32})));
        Modelica_LinearSystems2.Controller.StateSpace stateSpace(system=ss)
          annotation (Placement(transformation(extent={{-40,-10},{-20,10}})));
      equation
        connect(multiplex2_1.u1[1], stepEfd.y) annotation (Line(points={{-82,6},{-90,6},
                {-90,32},{-99,32}}, color={0,0,127}));
        connect(demultiplex2_2.y1[1], Vt) annotation (Line(points={{102,16},{108,16},{
                108,79},{150,79}},
                               color={0,0,127}));
        connect(demultiplex2_2.y4[1], w) annotation (Line(points={{102,-8},{110,-8},{110,
                -39},{150,-39}}, color={0,0,127}));
        connect(demultiplex2_2.y5[1], delta) annotation (Line(points={{102,-16},{108,-16},
                {108,-81},{150,-81}}, color={0,0,127}));
        connect(addy.y, demultiplex2_2.u)
          annotation (Line(points={{27,-6},{28,-6},{28,0},{56,0}}, color={0,0,127}));
        connect(Pmchange.y, multiplex2_1.u2[1]) annotation (Line(points={{-99,-30},{-90,
                -30},{-90,-6},{-82,-6}}, color={0,0,127}));
        connect(demultiplex2_2.y3[1], P) annotation (Line(points={{102,0},{122,0},{122,
                41},{150,41}}, color={0,0,127}));
        connect(Q, demultiplex2_2.y2[1]) annotation (Line(points={{150,1},{126,1},{126,
                8},{102,8}}, color={0,0,127}));
        connect(y0_initial.y, addy.u2)
          annotation (Line(points={{0,-21},{0,-12},{4,-12}}, color={0,0,127}));
        connect(multiplex2_1.y, stateSpace.u)
          annotation (Line(points={{-59,0},{-42,0}}, color={0,0,127}));
        connect(stateSpace.y, addy.u1)
          annotation (Line(points={{-19,0},{4,0}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false), graphics={
              Text(
                extent={{-58,24},{-44,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="du",
                fontSize=24),
              Text(
                extent={{-12,20},{-2,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="dy",
                fontSize=24),
              Text(
                extent={{-38,-18},{-22,-38}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                textString="y0",
                fontSize=24),
              Text(
                extent={{40,20},{50,4}},
                lineColor={238,46,47},
                fillPattern=FillPattern.VerticalCylinder,
                fillColor={255,0,0},
                fontSize=24,
                textString="y"),
              Text(
                extent={{-80,80},{0,60}},
                lineColor={85,170,255},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Note: the addy[] and y0_initial[] blocks 
are defined with ny, where ny is 
an integer of the size of the output matrix. 
This is visible in the Text layer only."),
              Text(
                extent={{-80,-80},{80,-100}},
                lineColor={85,170,255},
                fillPattern=FillPattern.HorizontalCylinder,
                fillColor={28,108,200},
                horizontalAlignment=TextAlignment.Left,
                textString="Notice the change in the order of the outputs w.r.t. the nonlinear model.
They have to be rearranged based on the order provided by the linearization function."),
              Line(
                points={{66,-86},{106,-84},{120,-4}},
                color={0,0,255},
                thickness=0.5,
                arrow={Arrow.None,Arrow.Filled},
                smooth=Smooth.Bezier),
              Line(
                points={{-4,68},{24,48},{18,18}},
                color={0,0,255},
                thickness=0.5,
                arrow={Arrow.None,Arrow.Filled},
                smooth=Smooth.Bezier)}),
          experiment(
            StopTime=15,
            __Dymola_NumberOfIntervals=1000,
            __Dymola_Algorithm="Dassl"),
          Documentation(info="<html>
<p>DO NOT try to run this model on it&apos;s own! </p>
<p>Models with this icon will not simulate on their own, instead they work together with a function that populates certain parameters in the model and perform other operations.</p>
<p><br>See the associated function to run:<span style=\"color: #1c6cc8;\"> <span style=\"font-family: Courier New; font-size: 8pt;\">LinearizeSMIBGeneral </span>within <span style=\"font-family: Courier New; font-size: 8pt; color: #1c6cc8;\">SMIB_PSControl.Analysis.LinearAnalysis.BasicLinearization.LinearizeSMIBGeneral()</span></p>
</html>"));
      end LinearModelGeneral;
    end Linearization;

    package PerturbationAnalysis
      "Examples that perform perturbations on the input of different model variants for comparison between linear and nonlinear models."
      extends Modelica.Icons.ExamplesPackage;
      model PerturbGen
        "Simulates the nonlinear SMIB model with a 1% step in the field voltage input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepEfd(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_GEN_wInput PS_ConstantEfd
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain efdInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-20})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-118,-30},{-98,-10}})));
      equation
        connect(Vt, PS_ConstantEfd.Vt) annotation (Line(points={{150,79},{98,
                79},{98,22.5714},{42.8571,22.5714}},
                                    color={0,0,127}));
        connect(P, PS_ConstantEfd.P) annotation (Line(points={{150,41},{112,
                41},{112,11.7143},{42.8571,11.7143}},
                                    color={0,0,127}));
        connect(Q, PS_ConstantEfd.Q) annotation (Line(points={{150,1},{96,1},
                {96,0.285714},{42.8571,0.285714}},
                                     color={0,0,127}));
        connect(w, PS_ConstantEfd.w) annotation (Line(points={{150,-39},{108,
                -39},{108,-11.1429},{42.8571,-11.1429}},
                                               color={0,0,127}));
        connect(delta, PS_ConstantEfd.delta) annotation (Line(points={{150,-81},
                {92,-81},{92,-23.1429},{42.8571,-23.1429}},
                                                   color={0,0,127}));
        connect(efdInputGain.u, stepEfd.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(pmInputGain.y, PS_ConstantEfd.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}}, color={0,0,127}));
        connect(efdInputGain.y, PS_ConstantEfd.uEfd) annotation (Line(points={{-59,20},
                {-52,20},{-52,11.4286},{-45.7143,11.4286}}, color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-97,-20},{-82,-20}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=15,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Radau"));
      end PerturbGen;

      model PerturbAVR
        "Simulates the nonlinear SMIB model with a 1% step in the Vs input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepVs(height=0.01,  startTime=1)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_AVR_wInput PS_wAVR
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain vsInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-20})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-120,-30},{-100,-10}})));
      equation
        connect(Vt, PS_wAVR.Vt) annotation (Line(points={{150,79},{98,79},{98,
                22.5714},{42.8571,22.5714}},
                                    color={0,0,127}));
        connect(P, PS_wAVR.P) annotation (Line(points={{150,41},{112,41},{112,
                11.7143},{42.8571,11.7143}},
                                    color={0,0,127}));
        connect(Q, PS_wAVR.Q) annotation (Line(points={{150,1},{96,1},{96,
                0.285714},{42.8571,0.285714}},
                            color={0,0,127}));
        connect(w, PS_wAVR.w) annotation (Line(points={{150,-39},{108,-39},{
                108,-11.1429},{42.8571,-11.1429}},
                                     color={0,0,127}));
        connect(delta, PS_wAVR.delta) annotation (Line(points={{150,-81},{92,
                -81},{92,-23.1429},{42.8571,-23.1429}},
                                               color={0,0,127}));
        connect(vsInputGain.u, stepVs.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(vsInputGain.y, PS_wAVR.uVref) annotation (Line(points={{-59,20},
                {-54,20},{-54,11.4286},{-45.7143,11.4286}},
                                                   color={0,0,127}));
        connect(pmInputGain.y, PS_wAVR.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}},
                                                     color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-99,-20},{-82,-20}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=15,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Rkfix4"));
      end PerturbAVR;

      model PerturbPSS
        "Simulates the nonlinear SMIB model with a 1% step in the PSS input."
        extends Modelica.Icons.Example;
        extends
          SMIBPS_IdControl.Analysis.LinearAnalysis.Interfaces.OutputsInterface;
        Modelica.Blocks.Sources.Step stepPSS(height=0.01, startTime=1)
          annotation (Placement(transformation(extent={{-120,10},{-100,30}})));
        Interfaces.SMIB_AVR_PSS_wInput PS_wPSS
          annotation (Placement(transformation(extent={{-40,-40},{40,40}})));
        Modelica.Blocks.Math.Gain pSSInputGain(k=1)  annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,20})));
        Modelica.Blocks.Math.Gain pmInputGain(k=1) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={-70,-20})));
        Modelica.Blocks.Sources.Constant Pmchange(k=0) annotation (Placement(
              transformation(extent={{-120,-30},{-100,-10}})));
      equation
        connect(Vt, PS_wPSS.Vt) annotation (Line(points={{150,79},{98,79},{98,
                22.5714},{42.8571,22.5714}},
                                    color={0,0,127}));
        connect(P, PS_wPSS.P) annotation (Line(points={{150,41},{112,41},{112,
                11.7143},{42.8571,11.7143}},
                                    color={0,0,127}));
        connect(Q, PS_wPSS.Q) annotation (Line(points={{150,1},{96,1},{96,
                0.285714},{42.8571,0.285714}},
                                     color={0,0,127}));
        connect(w, PS_wPSS.w) annotation (Line(points={{150,-39},{108,-39},{
                108,-11.1429},{42.8571,-11.1429}},
                                     color={0,0,127}));
        connect(delta, PS_wPSS.delta) annotation (Line(points={{150,-81},{92,
                -81},{92,-23.1429},{42.8571,-23.1429}},
                                               color={0,0,127}));
        connect(pSSInputGain.u, stepPSS.y)
          annotation (Line(points={{-82,20},{-99,20}}, color={0,0,127}));
        connect(pmInputGain.y, PS_wPSS.uPm) annotation (Line(points={{-59,-20},
                {-54,-20},{-54,-11.4286},{-45.7143,-11.4286}},
                                                          color={0,0,127}));
        connect(pSSInputGain.y, PS_wPSS.uPSS) annotation (Line(points={{-59,20},
                {-54,20},{-54,11.4286},{-45.7143,11.4286}},
                                                       color={0,0,127}));
        connect(Pmchange.y, pmInputGain.u)
          annotation (Line(points={{-99,-20},{-82,-20}}, color={0,0,127}));
        annotation (
          Icon(coordinateSystem(preserveAspectRatio=false)),
          Diagram(coordinateSystem(preserveAspectRatio=false)),
          experiment(
            StopTime=15,
            __Dymola_fixedstepsize=0.01,
            __Dymola_Algorithm="Radau"),
          Documentation(info="<html>
<p><span style=\"font-size: 10pt;\">This model applies a 1&percnt; step change in the PSS input signal.</span></p>
<p><span style=\"font-size: 10pt;\">It serves to compare the nonlinear response (this simulation) with other nonlinear models.</span></p>
<p><span style=\"font-size: 10pt;\">It also serves to compare it to the linearized model response.</span></p>
<p><span style=\"font-size: 10pt;\">It helps in understanding the automated analysis function which linearizes and compares all model variants.</span></p>
</html>"));
      end PerturbPSS;
    end PerturbationAnalysis;

  end LinearAnalysis;

end Analysis;
