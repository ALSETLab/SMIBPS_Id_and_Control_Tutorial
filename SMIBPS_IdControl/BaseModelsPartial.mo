within SMIBPS_IdControl;
package BaseModelsPartial "Partial Models - Cannot be simulated!"
  extends Modelica.Icons.BasesPackage;
  package BasePlants
    extends Modelica.Icons.BasesPackage;
    model Generator
      extends OpenIPSL.Electrical.Essentials.pfComponent;
      OpenIPSL.Electrical.Machines.PSAT.Order6 machine(
        Vn=400,
        V_b=V_b,
        ra=0.003,
        xd=1.81,
        xq=1.76,
        x1d=0.3,
        x1q=0.65,
        x2d=0.23,
        x2q=0.25,
        T1d0=8,
        T1q0=1,
        T2d0=0.03,
        T2q0=0.07,
        M=7,
        D=0,
        P_0=P_0,
        Q_0=Q_0,
        V_0=V_0,
        angle_0=angle_0,
        Sn=2220,
        Taa=0) annotation (Placement(transformation(extent={{14,-30},{74,30}})));
      OpenIPSL.Interfaces.PwPin pwPin annotation (Placement(transformation(extent={
                {100,-10},{120,10}}), iconTransformation(extent={{100,-10},{120,10}})));
    equation
      connect(machine.pm0, machine.pm) annotation (Line(points={{20,-33},{20,
              -33},{20,-40},{0,-40},{0,-15},{8,-15}},
                                                 color={0,0,127}));
      connect(machine.p, pwPin) annotation (Line(points={{74,0},{78.5,0},{
              78.5,0},{110,0}},  color={0,0,255}));
      connect(machine.vf0, machine.vf) annotation (Line(points={{20,33},{20,
              48},{-8,48},{-8,15},{8,15}},
                                    color={0,0,127}));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),Line(
              points={{-48,2},{-20,56},{2,4},{24,-28},{48,22}},
              color={0,0,0},
              smooth=Smooth.Bezier),Text(
              extent={{-52,-18},{56,-66}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name")}),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}})),
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
    end Generator;

    model Generator_wInputs
      extends OpenIPSL.Electrical.Essentials.pfComponent;
      OpenIPSL.Electrical.Machines.PSAT.Order6 machine(
        Vn=400,
        V_b=V_b,
        ra=0.003,
        xd=1.81,
        xq=1.76,
        x1d=0.3,
        x1q=0.65,
        x2d=0.23,
        x2q=0.25,
        T1d0=8,
        T1q0=1,
        T2d0=0.03,
        T2q0=0.07,
        M=7,
        D=0,
        P_0=P_0,
        Q_0=Q_0,
        V_0=V_0,
        angle_0=angle_0,
        Sn=2220,
        Taa=0) annotation (Placement(transformation(extent={{-20,-52},{80,48}})));
      OpenIPSL.Interfaces.PwPin pwPin annotation (Placement(transformation(extent={
                {100,-10},{120,10}}), iconTransformation(extent={{100,-10},{120,10}})));
      Modelica.Blocks.Math.Gain efdInputGain(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-34,70})));
      Modelica.Blocks.Math.Feedback efd_fdbck annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-62,24})));
      Modelica.Blocks.Interfaces.RealInput efd
        annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
      Modelica.Blocks.Interfaces.RealInput pm annotation (Placement(
            transformation(extent={{-140,-80},{-100,-40}})));
      Modelica.Blocks.Math.Gain pmInputGain(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-34,-82})));
      Modelica.Blocks.Math.Feedback pm_fdbck annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-64,-28})));
    equation
      connect(machine.p, pwPin) annotation (Line(points={{80,-2},{78.5,-2},{
              78.5,0},{110,0}},  color={0,0,255}));
      connect(efdInputGain.u, machine.vf0) annotation (Line(points={{-22,70},
              {-16,70},{-16,53},{-10,53}}, color={0,0,127}));
      connect(efd_fdbck.y, machine.vf) annotation (Line(points={{-53,24},{-42,
              24},{-42,23},{-30,23}}, color={0,0,127}));
      connect(efd_fdbck.u1, efd) annotation (Line(points={{-70,24},{-88,24},{
              -88,60},{-120,60}}, color={0,0,127}));
      connect(efdInputGain.y, efd_fdbck.u2) annotation (Line(points={{-45,70},
              {-80,70},{-80,0},{-62,0},{-62,16}}, color={0,0,127}));
      connect(pm_fdbck.y, machine.pm) annotation (Line(points={{-55,-28},{-42,
              -28},{-42,-27},{-30,-27}}, color={0,0,127}));
      connect(pmInputGain.u, machine.pm0) annotation (Line(points={{-22,-82},
              {-18,-82},{-18,-57},{-10,-57}}, color={0,0,127}));
      connect(pmInputGain.y, pm_fdbck.u2) annotation (Line(points={{-45,-82},
              {-54,-82},{-54,-36},{-64,-36}}, color={0,0,127}));
      connect(pm, pm_fdbck.u1) annotation (Line(points={{-120,-60},{-86,-60},
              {-86,-28},{-72,-28}}, color={0,0,127}));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),Line(
              points={{-48,2},{-20,56},{2,4},{24,-28},{48,22}},
              color={0,0,0},
              smooth=Smooth.Bezier),Text(
              extent={{-52,-18},{56,-66}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name")}),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}})),
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
    end Generator_wInputs;

    model Generator_AVR
      extends OpenIPSL.Electrical.Essentials.pfComponent;
      OpenIPSL.Electrical.Machines.PSAT.Order6 machine(
        Vn=400,
        V_b=V_b,
        ra=0.003,
        xd=1.81,
        xq=1.76,
        x1d=0.3,
        x1q=0.65,
        x2d=0.23,
        x2q=0.25,
        T1d0=8,
        T1q0=1,
        T2d0=0.03,
        T2q0=0.07,
        M=7,
        D=0,
        P_0=P_0,
        Q_0=Q_0,
        V_0=V_0,
        angle_0=angle_0,
        Sn=2220,
        Taa=0) annotation (Placement(transformation(extent={{14,-30},{74,30}})));
      OpenIPSL.Interfaces.PwPin pwPin annotation (Placement(transformation(extent={
                {100,-10},{120,10}}), iconTransformation(extent={{100,-10},{120,10}})));
      OpenIPSL.Electrical.Controls.PSAT.AVR.AVRtypeIII avr(
        vfmax=7,
        vfmin=-6.40,
        K0=200,
        T2=1,
        T1=1,
        Te=0.0001,
        Tr=0.015) annotation (Placement(transformation(extent={{-52,-4},{-12,36}})));
      Modelica.Blocks.Sources.Constant pss_off(k=0)
        annotation (Placement(transformation(extent={{-92,-2},{-72,18}})));
    equation
      connect(machine.p, pwPin)
        annotation (Line(points={{74,0},{78.5,0},{110,0}}, color={0,0,255}));
      connect(pss_off.y, avr.vs) annotation (Line(points={{-71,8},{-50.3333,8},
              {-50.3333,6}},
                   color={0,0,127}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,16},{2,16},
              {2,15},{8,15}},
                           color={0,0,127}));
      connect(machine.v, avr.v) annotation (Line(points={{77,9},{88,9},{88,52},
              {-50.3333,52},{-50.3333,26}},
                                  color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{4,33},{4,
              44},{-30,44},{-30,34.3333},{-32,34.3333}},
                                                    color={0,0,127}));
      connect(machine.pm0, machine.pm) annotation (Line(points={{20,-33},{20,
              -40},{-10,-40},{-10,-15},{8,-15}}, color={0,0,127}));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),Line(
              points={{-48,2},{-20,56},{2,4},{24,-28},{48,22}},
              color={0,0,0},
              smooth=Smooth.Bezier),Text(
              extent={{-52,-18},{56,-66}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name")}),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}})),
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
    end Generator_AVR;

    model Generator_AVR_wInputs
      extends OpenIPSL.Electrical.Essentials.pfComponent;
      OpenIPSL.Electrical.Machines.PSAT.Order6 machine(
        Vn=400,
        V_b=V_b,
        ra=0.003,
        xd=1.81,
        xq=1.76,
        x1d=0.3,
        x1q=0.65,
        x2d=0.23,
        x2q=0.25,
        T1d0=8,
        T1q0=1,
        T2d0=0.03,
        T2q0=0.07,
        M=7,
        D=0,
        P_0=P_0,
        Q_0=Q_0,
        V_0=V_0,
        angle_0=angle_0,
        Sn=2220,
        Taa=0) annotation (Placement(transformation(extent={{14,-36},{74,24}})));
      OpenIPSL.Interfaces.PwPin pwPin annotation (Placement(transformation(extent={
                {100,-10},{120,10}}), iconTransformation(extent={{100,-10},{120,10}})));
      OpenIPSL.Electrical.Controls.PSAT.AVR.AVRtypeIII avr(
        vfmax=7,
        vfmin=-6.40,
        K0=200,
        T2=1,
        T1=1,
        Te=0.0001,
        Tr=0.015) annotation (Placement(transformation(extent={{-52,-10},{-12,30}})));
      Modelica.Blocks.Interfaces.RealInput uVs annotation (Placement(
            transformation(extent={{-142,40},{-102,80}}), iconTransformation(
              extent={{-142,40},{-102,80}})));
      Modelica.Blocks.Math.Gain pmInputGain(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-32,-80})));
      Modelica.Blocks.Math.Feedback pm_fdbck annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-66,-28})));
      Modelica.Blocks.Interfaces.RealInput pm annotation (Placement(
            transformation(extent={{-140,-80},{-100,-40}})));
    equation
      connect(machine.p, pwPin)
        annotation (Line(points={{74,-6},{92,-6},{92,0},{110,0}},
                                                           color={0,0,255}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,10},{2,10},
              {2,9},{8,9}},color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,27},{20,38},{
              -30,38},{-30,28.3333},{-32,28.3333}}, color={0,0,127}));
      connect(machine.v, avr.v) annotation (Line(points={{77,3},{86,3},{86,60},
              {-64,60},{-64,20},{-50.3333,20}},   color={0,0,127}));
      connect(avr.vs, uVs) annotation (Line(points={{-50.3333,0},{-86,0},{-86,
              60},{-122,60}}, color={0,0,127}));
      connect(pm_fdbck.y, machine.pm) annotation (Line(points={{-57,-28},{-44,
              -28},{-44,-21},{8,-21}}, color={0,0,127}));
      connect(pmInputGain.y, pm_fdbck.u2) annotation (Line(points={{-43,-80},
              {-56,-80},{-56,-36},{-66,-36}}, color={0,0,127}));
      connect(pm, pm_fdbck.u1) annotation (Line(points={{-120,-60},{-88,-60},
              {-88,-28},{-74,-28}}, color={0,0,127}));
      connect(pmInputGain.u, machine.pm0) annotation (Line(points={{-20,-80},
              {20,-80},{20,-39}}, color={0,0,127}));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),Line(
              points={{-48,2},{-20,56},{2,4},{24,-28},{48,22}},
              color={0,0,0},
              smooth=Smooth.Bezier),Text(
              extent={{-52,-18},{56,-66}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name")}),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}})),
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
    end Generator_AVR_wInputs;

    model Generator_AVR_PSS
      extends OpenIPSL.Electrical.Essentials.pfComponent;
      OpenIPSL.Electrical.Machines.PSAT.Order6 machine(
        Vn=400,
        V_b=V_b,
        ra=0.003,
        xd=1.81,
        xq=1.76,
        x1d=0.3,
        x1q=0.65,
        x2d=0.23,
        x2q=0.25,
        T1d0=8,
        T1q0=1,
        T2d0=0.03,
        T2q0=0.07,
        M=7,
        D=0,
        P_0=P_0,
        Q_0=Q_0,
        V_0=V_0,
        angle_0=angle_0,
        Sn=2220,
        Taa=0) annotation (Placement(transformation(extent={{14,-30},{74,30}})));
      OpenIPSL.Interfaces.PwPin pwPin annotation (Placement(transformation(extent={
                {100,-10},{120,10}}), iconTransformation(extent={{100,-10},{120,10}})));
      OpenIPSL.Electrical.Controls.PSAT.AVR.AVRtypeIII avr(
        vfmax=7,
        vfmin=-6.40,
        K0=200,
        T2=1,
        T1=1,
        Te=0.0001,
        Tr=0.015) annotation (Placement(transformation(extent={{-52,-4},{-12,36}})));
      OpenIPSL.Electrical.Controls.PSAT.PSS.PSSTypeII pss(
        vsmax=0.2,
        vsmin=-0.2,
        Kw=9.5,
        Tw=1.41,
        T1=0.154,
        T2=0.033,
        T3=1,
        T4=1) annotation (Placement(transformation(extent={{-84,-2},{-64,18}})));
    equation
      connect(machine.pm0, machine.pm) annotation (Line(points={{20,-33},{20,-33},{
              20,-40},{0,-40},{0,-15},{8,-15}},  color={0,0,127}));
      connect(machine.p, pwPin) annotation (Line(points={{74,0},{78.5,0},{78.5,0},{
              110,0}},           color={0,0,255}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,16},{2,16},
              {2,15},{8,15}},
                       color={0,0,127}));
      connect(machine.v, avr.v) annotation (Line(points={{77,9},{88,9},{88,52},
              {-50.3333,52},{-50.3333,26}},
                             color={0,0,127}));
      connect(pss.vs, avr.vs)
        annotation (Line(points={{-63,8},{-63,6},{-50.3333,6}},
                                                           color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{-6,33},{
              -6,44},{-32,44},{-32,34.3333}},
                                       color={0,0,127}));
      connect(machine.w, pss.vSI) annotation (Line(points={{77,27},{77,-54},{-98,
              -54},{-98,8},{-85,8}}, color={0,0,127}));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}), graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),Line(
              points={{-48,2},{-20,56},{2,4},{24,-28},{48,22}},
              color={0,0,0},
              smooth=Smooth.Bezier),Text(
              extent={{-52,-18},{56,-66}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name")}),
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}})),
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
    end Generator_AVR_PSS;

    model Generator_AVR_PSS_wInputs
      extends OpenIPSL.Electrical.Essentials.pfComponent;
      OpenIPSL.Electrical.Machines.PSAT.Order6 machine(
        Vn=400,
        V_b=V_b,
        ra=0.003,
        xd=1.81,
        xq=1.76,
        x1d=0.3,
        x1q=0.65,
        x2d=0.23,
        x2q=0.25,
        T1d0=8,
        T1q0=1,
        T2d0=0.03,
        T2q0=0.07,
        M=7,
        D=0,
        P_0=P_0,
        Q_0=Q_0,
        V_0=V_0,
        angle_0=angle_0,
        Sn=2220,
        Taa=0) annotation (Placement(transformation(extent={{14,-30},{74,30}})));
      OpenIPSL.Interfaces.PwPin pwPin annotation (Placement(transformation(extent={
                {100,-10},{120,10}}), iconTransformation(extent={{100,-10},{120,10}})));
      OpenIPSL.Electrical.Controls.PSAT.AVR.AVRtypeIII avr(
        vfmax=7,
        vfmin=-6.40,
        K0=200,
        T2=1,
        T1=1,
        Te=0.0001,
        Tr=0.015) annotation (Placement(transformation(extent={{-52,-4},{-12,36}})));
      OpenIPSL.Electrical.Controls.PSAT.PSS.PSSTypeII pss(
        vsmax=0.2,
        vsmin=-0.2,
        Kw=9.5,
        Tw=1.41,
        T1=0,
        T2=0,
        T3=0,
        T4=0) annotation (Placement(transformation(extent={{-76,-4},{-56,16}})));
      Modelica.Blocks.Interfaces.RealInput uPSS annotation (Placement(
            transformation(extent={{-140,40},{-100,80}}), iconTransformation(
              extent={{-140,40},{-100,80}})));
      Modelica.Blocks.Math.Feedback feedback
        annotation (Placement(transformation(extent={{-100,-4},{-80,16}})));
      Modelica.Blocks.Math.Gain gain(k=-1) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={0,84})));
      Modelica.Blocks.Math.Gain pmInputGain(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-32,-80})));
      Modelica.Blocks.Math.Feedback pm_fdbck annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-66,-28})));
      Modelica.Blocks.Interfaces.RealInput pm annotation (Placement(
            transformation(extent={{-140,-80},{-100,-40}})));
    equation
      connect(machine.p, pwPin) annotation (Line(points={{74,0},{78.5,0},{78.5,0},{
              110,0}},           color={0,0,255}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,16},{2,
              16},{2,15},{8,15}},
                       color={0,0,127}));
      connect(machine.v, avr.v) annotation (Line(points={{77,9},{98,9},{98,54},
              {-50.3333,54},{-50.3333,26}},
                             color={0,0,127}));
      connect(pss.vs, avr.vs)
        annotation (Line(points={{-55,6},{-50.3333,6}},    color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{-6,33},
              {-6,44},{-32,44},{-32,34.3333}},
                                       color={0,0,127}));
      connect(uPSS, feedback.u1) annotation (Line(points={{-120,60},{-110,60},
              {-110,6},{-98,6}}, color={0,0,127}));
      connect(feedback.y, pss.vSI)
        annotation (Line(points={{-81,6},{-77,6}},  color={0,0,127}));
      connect(machine.w, gain.u) annotation (Line(points={{77,27},{94,27},{94,
              84},{12,84}},
                          color={0,0,127}));
      connect(gain.y, feedback.u2)
        annotation (Line(points={{-11,84},{-90,84},{-90,-2}},    color={0,0,127}));
      connect(pm_fdbck.y, machine.pm) annotation (Line(points={{-57,-28},{8,
              -28},{8,-15}}, color={0,0,127}));
      connect(pmInputGain.y, pm_fdbck.u2) annotation (Line(points={{-43,-80},
              {-56,-80},{-56,-36},{-66,-36}}, color={0,0,127}));
      connect(pm, pm_fdbck.u1) annotation (Line(points={{-120,-60},{-88,-60},
              {-88,-28},{-74,-28}}, color={0,0,127}));
      connect(pmInputGain.u, machine.pm0) annotation (Line(points={{-20,-80},
              {-20,-33},{20,-33}}, color={0,0,127}));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false),
                        graphics={Ellipse(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid),Line(
              points={{-48,2},{-20,56},{2,4},{24,-28},{48,22}},
              color={0,0,0},
              smooth=Smooth.Bezier),Text(
              extent={{-52,-18},{56,-66}},
              lineColor={0,0,0},
              fillColor={255,255,255},
              fillPattern=FillPattern.Solid,
              textString="%name")}),
        Diagram(coordinateSystem(preserveAspectRatio=false)),
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
    end Generator_AVR_PSS_wInputs;
  annotation (Documentation(info="<html>
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
  end BasePlants;

  package BaseNetwork
    extends Modelica.Icons.BasesPackage;

    partial model SMIB_Base "Partial model containing the network elements"
      import Modelica.Constants.pi;
      OpenIPSL.Electrical.Branches.PSAT.TwoWindingTransformer transformer(
        Sn=2220,
        x=0.15,
        r=0,
        V_b=400,
        Vn=400) annotation (Placement(transformation(extent={{-66,-10},{-46,10}})));
      OpenIPSL.Electrical.Branches.PwLine line_1(
        R=0,
        G=0,
        B=0,
        X=0.5) annotation (Placement(transformation(extent={{22,14},{40,26}})));
      OpenIPSL.Electrical.Buses.InfiniteBus infinite_bus(angle_0=0, V_0=
            0.900810000000000) annotation (Placement(transformation(
            extent={{10,10},{-10,-10}},
            rotation=0,
            origin={110,0})));
      OpenIPSL.Electrical.Branches.PwLine line_2(
        R=0,
        G=0,
        B=0,
        X=0.93/2,
        opening=1)
        annotation (Placement(transformation(extent={{-2,-46},{16,-34}})));
      inner OpenIPSL.Electrical.SystemBase SysData(S_b=2220, fn=60)
        annotation (Placement(transformation(extent={{-140,80},{-86,100}})));
      OpenIPSL.Electrical.Branches.PwLine line_3(
        R=0,
        G=0,
        B=0,
        X=0.93/2,
        opening=1)
        annotation (Placement(transformation(extent={{44,-46},{62,-34}})));
      OpenIPSL.Electrical.Buses.Bus    B1(V_b=400)
        annotation (Placement(transformation(extent={{-90,-10},{-70,10}})));
      OpenIPSL.Electrical.Buses.Bus    B2(
        V_b=400)
        annotation (Placement(transformation(extent={{10,10},{-10,-10}},
            rotation=180,
            origin={-30,0})));
      OpenIPSL.Electrical.Buses.Bus    B3(V_b=400)
        annotation (Placement(transformation(extent={{10,10},{-10,-10}},
            rotation=180,
            origin={82,0})));
      OpenIPSL.Electrical.Buses.Bus    B4(
        V_b=400)
              annotation (Placement(transformation(extent={{18,-50},{38,-30}})));
    protected
      parameter Real S_b=SysData.S_b;
    equation
      connect(line_2.p, line_1.p) annotation (Line(points={{-1.1,-40},{-10,-40},
              {-10,20},{22.9,20}},
                             color={0,0,255}));
      connect(transformer.n, B2.p) annotation (Line(points={{-45,0},{-38,0},{
              -38,4.44089e-16},{-30,4.44089e-16}}, color={0,0,255}));
      connect(B2.p, line_1.p) annotation (Line(points={{-30,0},{-10,0},{-10,20},
              {22.9,20}}, color={0,0,255}));
      connect(B3.p, infinite_bus.p) annotation (Line(points={{82,4.44089e-16},{
              81,4.44089e-16},{81,0},{100,0}}, color={0,0,255}));
      connect(line_1.n, B3.p) annotation (Line(points={{39.1,20},{70,20},{70,0},
              {82,0},{82,4.44089e-16}}, color={0,0,255}));
      connect(line_3.n, B3.p) annotation (Line(points={{61.1,-40},{70,-40},{70,
              4.44089e-16},{82,4.44089e-16}}, color={0,0,255}));
      connect(B4.p, line_3.p)
        annotation (Line(points={{28,-40},{44.9,-40}}, color={0,0,255}));
      connect(B4.p, line_2.n)
        annotation (Line(points={{28,-40},{15.1,-40}}, color={0,0,255}));
      connect(B1.p, transformer.p)
        annotation (Line(points={{-80,0},{-67,0}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(extent={{-140,-100},{120,100}},
              preserveAspectRatio=false), graphics={Text(
              extent={{-110,80},{110,60}},
              lineColor={0,0,0},
              lineThickness=1,
              fontSize=15,
              textStyle={TextStyle.Bold},
              textString="Single-machine infinite bus model*")}),
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
    end SMIB_Base;

    partial model SMIB_BaseWithPF "Partial SMIB Model with a fault block"
      extends SMIB_Base(
        B2(
          V_0=powerFlow_Data.bus.V2,
          angle_0=powerFlow_Data.bus.A2),
        infinite_bus(
          V_0=powerFlow_Data.bus.V3,
          angle_0=powerFlow_Data.bus.A3,
          P_0=powerFlow_Data.machines.PG2,
          Q_0=powerFlow_Data.machines.QG2));
      OpenIPSL.Electrical.Loads.PSSE.Load_ExtInput load_ExtInput(
        V_0=powerFlow_Data.bus.V4,
        angle_0=powerFlow_Data.bus.A4,
        P_0=powerFlow_Data.loads.PL1,
        Q_0=powerFlow_Data.loads.QL1,
        d_P=0,
        t1=0,
        d_t=0)
        annotation (Placement(transformation(extent={{16,-76},{28,-64}})));
      PF_Data.PowerFlow_Data powerFlow_Data
        annotation (Placement(transformation(extent={{-120,44},{-100,64}})));
    equation
      connect(load_ExtInput.p, line_2.n)
        annotation (Line(points={{22,-64},{22,-40},{15.1,-40}}, color={0,0,255}));
    end SMIB_BaseWithPF;

    partial model SMIB_Partial "Partial SMIB Model with a fault block"
      extends SMIB_Base(
        B2(
          V_0=powerFlow_Data.bus.V2,
          angle_0=powerFlow_Data.bus.A2),
        line_2(t1=0.57),
        infinite_bus(
          V_0=powerFlow_Data.bus.V3,
          angle_0=powerFlow_Data.bus.A3,
          P_0=powerFlow_Data.machines.PG2,
          Q_0=powerFlow_Data.machines.QG2));
      OpenIPSL.Electrical.Events.PwFault fault(
        R=0,
        t1=0.5,
        t2=0.57,
        X=1e-5) annotation (Placement(transformation(extent={{-6,-6},{6,6}},
            rotation=270,
            origin={-20,-64})));
      OpenIPSL.Electrical.Loads.PSSE.Load_ExtInput load_ExtInput(
        V_0=powerFlow_Data.bus.V4,
        angle_0=powerFlow_Data.bus.A4,
        P_0=powerFlow_Data.loads.PL1,
        Q_0=powerFlow_Data.loads.QL1)
        annotation (Placement(transformation(extent={{16,-76},{28,-64}})));
      PF_Data.PowerFlow_Data powerFlow_Data
        annotation (Placement(transformation(extent={{-120,44},{-100,64}})));
    equation
      connect(fault.p, B2.p)
        annotation (Line(points={{-20,-57},{-20,0},{-30,0}}, color={0,0,255}));
      connect(load_ExtInput.p, line_2.n)
        annotation (Line(points={{22,-64},{22,-40},{15.1,-40}}, color={0,0,255}));
    end SMIB_Partial;
  end BaseNetwork;
end BaseModelsPartial;
