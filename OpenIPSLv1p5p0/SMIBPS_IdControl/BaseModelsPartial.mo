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
      connect(machine.w, pss.vSI) annotation (Line(points={{77,27},{78,27},{78,
              28},{90,28},{90,-56},{-98,-56},{-98,8},{-85,8}},
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
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,16},{2,16},
              {2,15},{8,15}},
                       color={0,0,127}));
      connect(machine.v, avr.v) annotation (Line(points={{77,9},{98,9},{98,54},
              {-50.3333,54},{-50.3333,26}},
                             color={0,0,127}));
      connect(pss.vs, avr.vs)
        annotation (Line(points={{-55,6},{-50.3333,6}},    color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{-6,33},{
              -6,44},{-32,44},{-32,34.3333}},
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
</html>"),
        __Dymola_Commands(file=
              "MosScripts/nonlinear_simulation_line_removal_gen_output.mos"
            "genoutput", file="MosScripts/genoutput.mos" "genoutput"));
    end Generator_AVR_PSS_wInputs;

    model Generator_AVR_PSS_w3Inputs
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
        Tr=0.015) annotation (Placement(transformation(extent={{-46,-4},{-6,36}})));
      OpenIPSL.Electrical.Controls.PSAT.PSS.PSSTypeII pss(
        vsmax=0.2,
        vsmin=-0.2,
        Kw=9.5,
        Tw=1.41,
        T1=0,
        T2=0,
        T3=0,
        T4=0) annotation (Placement(transformation(extent={{-112,-4},{-92,16}})));
      Modelica.Blocks.Interfaces.RealInput uPSS annotation (Placement(
            transformation(extent={{-140,40},{-100,80}}), iconTransformation(
              extent={{-140,40},{-100,80}})));
      Modelica.Blocks.Math.Feedback feedbackPSS annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-126,6})));
      Modelica.Blocks.Math.Gain gain_uPSS(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={2,70})));
      Modelica.Blocks.Math.Gain pmInputGain(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-26,-44})));
      Modelica.Blocks.Math.Feedback pm_fdbck annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-66,-28})));
      Modelica.Blocks.Interfaces.RealInput upm
        annotation (Placement(transformation(extent={{-140,-80},{-100,-40}})));
      Modelica.Blocks.Math.Feedback feedbackAVR annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-64,6})));
      Modelica.Blocks.Math.Gain gain_uAVR(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-44,-80})));
      Modelica.Blocks.Interfaces.RealInput uvs annotation (Placement(
            transformation(
            extent={{-20,-20},{20,20}},
            rotation=90,
            origin={0,-120})));
    equation
      connect(machine.p, pwPin) annotation (Line(points={{74,0},{110,0}},
                                 color={0,0,255}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-4.33333,16},{2,16},
              {2,15},{8,15}},
                       color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{20,48},{
              -26,48},{-26,34.3333}},  color={0,0,127}));
      connect(feedbackPSS.y, pss.vSI)
        annotation (Line(points={{-117,6},{-113,6}}, color={0,0,127}));
      connect(machine.w, gain_uPSS.u) annotation (Line(points={{77,27},{94,27},
              {94,70},{14,70}}, color={0,0,127}));
      connect(pm_fdbck.y, machine.pm) annotation (Line(points={{-57,-28},{8,-28},
              {8,-15}},      color={0,0,127}));
      connect(pmInputGain.y, pm_fdbck.u2) annotation (Line(points={{-37,-44},{
              -66,-44},{-66,-36}},            color={0,0,127}));
      connect(upm, pm_fdbck.u1) annotation (Line(points={{-120,-60},{-88,-60},{
              -88,-28},{-74,-28}}, color={0,0,127}));
      connect(pmInputGain.u, machine.pm0) annotation (Line(points={{-14,-44},{
              20,-44},{20,-33}},   color={0,0,127}));
      connect(pss.vs, feedbackAVR.u1)
        annotation (Line(points={{-91,6},{-72,6}}, color={0,0,127}));
      connect(avr.vs, feedbackAVR.y)
        annotation (Line(points={{-44.3333,6},{-55,6}}, color={0,0,127}));
      connect(avr.v, machine.v) annotation (Line(points={{-44.3333,26},{-54,26},
              {-54,54},{86,54},{86,9},{77,9}}, color={0,0,127}));
      connect(gain_uPSS.y, feedbackPSS.u2) annotation (Line(points={{-9,70},{
              -86,70},{-86,26},{-156,26},{-156,-18},{-126,-18},{-126,-2}},
            color={0,0,127}));
      connect(uPSS, feedbackPSS.u1) annotation (Line(points={{-120,60},{-96,60},
              {-96,38},{-148,38},{-148,6},{-134,6}}, color={0,0,127}));
      connect(gain_uAVR.y, feedbackAVR.u2) annotation (Line(points={{-55,-80},{
              -80,-80},{-80,-8},{-64,-8},{-64,-2}}, color={0,0,127}));
      connect(uvs, gain_uAVR.u) annotation (Line(points={{0,-120},{0,-80},{-32,
              -80}}, color={0,0,127}));
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
</html>"),
        __Dymola_Commands(file=
              "MosScripts/nonlinear_simulation_line_removal_gen_output.mos"
            "genoutput", file="MosScripts/genoutput.mos" "genoutput"));
    end Generator_AVR_PSS_w3Inputs;

    model Generator_AVR_PSS_wInputs_propagate
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
        Kw=Kw,
        Tw=Tw,
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
      parameter Real Kw=9.5 "Stabilizer gain (pu/pu)";
      parameter Real Tw=1.41 "Wash-out time constant (s)";
    equation
      connect(machine.p, pwPin) annotation (Line(points={{74,0},{78.5,0},{78.5,0},{
              110,0}},           color={0,0,255}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,16},{2,16},
              {2,15},{8,15}},
                       color={0,0,127}));
      connect(machine.v, avr.v) annotation (Line(points={{77,9},{98,9},{98,54},
              {-50.3333,54},{-50.3333,26}},
                             color={0,0,127}));
      connect(pss.vs, avr.vs)
        annotation (Line(points={{-55,6},{-50.3333,6}},    color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{-6,33},{
              -6,44},{-32,44},{-32,34.3333}},
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
</html>"),
        __Dymola_Commands(file=
              "MosScripts/nonlinear_simulation_line_removal_gen_output.mos"
            "genoutput", file="MosScripts/genoutput.mos" "genoutput"));
    end Generator_AVR_PSS_wInputs_propagate;

    model Generator_AVR_PSS_w3InputsMoreOutputs_propagate
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
        vfmax=vfmax,
        vfmin=vfmin,
        K0=K0,
        T2=1,
        T1=1,
        Te=0.0001,
        Tr=0.015) annotation (Placement(transformation(extent={{-52,-4},{-12,36}})));
      OpenIPSL.Electrical.Controls.PSAT.PSS.PSSTypeII pss(
        vsmax=0.2,
        vsmin=-0.2,
        Kw=Kw,
        Tw=Tw,
        T1=T1,
        T2=T2,
        T3=T3,
        T4=T4)
              annotation (Placement(transformation(extent={{-114,-4},{-94,16}})));
      Modelica.Blocks.Interfaces.RealInput uPSS annotation (Placement(
            transformation(extent={{-140,40},{-100,80}}), iconTransformation(
              extent={{-140,40},{-100,80}})));
      Modelica.Blocks.Math.Feedback feedbackPSS annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-130,6})));
      Modelica.Blocks.Math.Gain gain_uPSS(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={2,70})));
      Modelica.Blocks.Math.Gain pmInputGain(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-26,-44})));
      Modelica.Blocks.Math.Feedback pm_fdbck annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-66,-28})));
      Modelica.Blocks.Interfaces.RealInput upm
        annotation (Placement(transformation(extent={{-140,-80},{-100,-40}}),
            iconTransformation(extent={{-140,-80},{-100,-40}})));
      Modelica.Blocks.Math.Feedback feedbackAVR annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-80,6})));
      Modelica.Blocks.Math.Gain gain_uAVR(k=-1) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={-44,-80})));
      Modelica.Blocks.Interfaces.RealInput uvsAVR annotation (Placement(
            transformation(
            extent={{-20,-20},{20,20}},
            rotation=90,
            origin={0,-120})));
      parameter Real Kw=9.5 "Stabilizer gain (pu/pu)" annotation (Dialog(group="PSS"));
      parameter Real Tw=1.41 "Wash-out time constant (s)" annotation (Dialog(group="PSS"));
      parameter Real T1=0 "First stabilizer time constant (s)" annotation (Dialog(group="PSS"));
      parameter Real T2=0 "Second stabilizer time constant (s)" annotation (Dialog(group="PSS"));
      parameter Real T3=0 "Third stabilizer time constant (s)" annotation (Dialog(group="PSS"));
      parameter Real T4=0 "Fourth stabilizer time constant (s)" annotation (Dialog(group="PSS"));
      parameter Real vfmax=7.0 "max lim." annotation (Dialog(group="AVR"));
      parameter Real vfmin=-6.40 "min lim." annotation (Dialog(group="AVR"));
      parameter Real K0=200 "regulator gain" annotation (Dialog(group="AVR"));
      Modelica.Blocks.Interfaces.RealOutput AVRout
        annotation (Placement(transformation(extent={{100,-90},{120,-70}})));
      Modelica.Blocks.Interfaces.RealOutput AVRin
        annotation (Placement(transformation(extent={{100,70},{120,90}})));
    equation
      connect(machine.p, pwPin) annotation (Line(points={{74,0},{110,0}},
                                 color={0,0,255}));
      connect(avr.vf, machine.vf) annotation (Line(points={{-10.3333,16},{2,16},
              {2,15},{8,15}},
                       color={0,0,127}));
      connect(machine.vf0, avr.vf0) annotation (Line(points={{20,33},{20,48},{
              -32,48},{-32,34.3333}},  color={0,0,127}));
      connect(feedbackPSS.y, pss.vSI)
        annotation (Line(points={{-121,6},{-115,6}}, color={0,0,127}));
      connect(machine.w, gain_uPSS.u) annotation (Line(points={{77,27},{94,27},
              {94,70},{14,70}}, color={0,0,127}));
      connect(pm_fdbck.y, machine.pm) annotation (Line(points={{-57,-28},{8,-28},
              {8,-15}},      color={0,0,127}));
      connect(pmInputGain.y, pm_fdbck.u2) annotation (Line(points={{-37,-44},{
              -66,-44},{-66,-36}},            color={0,0,127}));
      connect(upm, pm_fdbck.u1) annotation (Line(points={{-120,-60},{-98,-60},{
              -98,-28},{-74,-28}}, color={0,0,127}));
      connect(pmInputGain.u, machine.pm0) annotation (Line(points={{-14,-44},{
              20,-44},{20,-33}},   color={0,0,127}));
      connect(pss.vs, feedbackAVR.u1)
        annotation (Line(points={{-93,6},{-88,6}}, color={0,0,127}));
      connect(avr.vs, feedbackAVR.y)
        annotation (Line(points={{-50.3333,6},{-71,6}}, color={0,0,127}));
      connect(avr.v, machine.v) annotation (Line(points={{-50.3333,26},{-60,26},
              {-60,54},{86,54},{86,9},{77,9}}, color={0,0,127}));
      connect(gain_uPSS.y, feedbackPSS.u2) annotation (Line(points={{-9,70},{
              -86,70},{-86,26},{-156,26},{-156,-18},{-130,-18},{-130,-2}},
            color={0,0,127}));
      connect(uPSS, feedbackPSS.u1) annotation (Line(points={{-120,60},{-160,60},
              {-160,6},{-138,6}},                    color={0,0,127}));
      connect(gain_uAVR.y, feedbackAVR.u2) annotation (Line(points={{-55,-80},{
              -80,-80},{-80,-2}},                   color={0,0,127}));
      connect(uvsAVR, gain_uAVR.u) annotation (Line(points={{0,-120},{0,-100},{
              -20,-100},{-20,-78},{-26,-78},{-26,-80},{-32,-80}},
                     color={0,0,127}));
      connect(AVRout, machine.vf) annotation (Line(
          points={{110,-80},{-6,-80},{-6,16},{2,16},{2,15},{8,15}},
          color={0,140,72},
          thickness=1));
      connect(AVRin, feedbackAVR.y) annotation (Line(
          points={{110,80},{58,80},{58,90},{-68,90},{-68,6},{-71,6}},
          color={217,67,180},
          thickness=1));
      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}}),
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
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-160,-100},
                {100,100}})),
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
</html>"),
        __Dymola_Commands(file=
              "MosScripts/nonlinear_simulation_line_removal_gen_output.mos"
            "genoutput", file="MosScripts/genoutput.mos" "genoutput"));
    end Generator_AVR_PSS_w3InputsMoreOutputs_propagate;
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
        X=0.93/2)
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
              preserveAspectRatio=false)),
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
          Q_0=powerFlow_Data.machines.QG2),
        B1(V_0=powerFlow_Data.bus.V1, angle_0=powerFlow_Data.bus.A1),
        B3(V_0=powerFlow_Data.bus.V3, angle_0=powerFlow_Data.bus.A3),
        B4(V_0=powerFlow_Data.bus.V4, angle_0=powerFlow_Data.bus.A4));
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
          Q_0=powerFlow_Data.machines.QG2),
        B1(V_0=powerFlow_Data.bus.V1, angle_0=powerFlow_Data.bus.A1),
        B3(V_0=powerFlow_Data.bus.V3, angle_0=powerFlow_Data.bus.A3),
        B4(V_0=powerFlow_Data.bus.V4, angle_0=powerFlow_Data.bus.A4));
      OpenIPSL.Electrical.Events.PwFault fault(
        R=0,
        t1=0.5,
        t2=0.57,
        X=1e-5) annotation (Placement(transformation(extent={{-6,-6},{6,6}},
            rotation=0,
            origin={32,-14})));
      OpenIPSL.Electrical.Loads.PSSE.Load_ExtInput load_ExtInput(
        V_0=powerFlow_Data.bus.V4,
        angle_0=powerFlow_Data.bus.A4,
        P_0=powerFlow_Data.loads.PL1,
        Q_0=powerFlow_Data.loads.QL1,
        d_P=0,
        t1=Modelica.Constants.inf,
        d_t=Modelica.Constants.inf)
        annotation (Placement(transformation(extent={{16,-76},{28,-64}})));
      PF_Data.PowerFlow_Data powerFlow_Data
        annotation (Placement(transformation(extent={{-120,44},{-100,64}})));
    equation
      connect(load_ExtInput.p, line_2.n)
        annotation (Line(points={{22,-64},{22,-40},{15.1,-40}}, color={0,0,255}));
      connect(fault.p, line_2.n) annotation (Line(points={{25,-14},{24,-14},{24,
              -40},{15.1,-40}}, color={0,0,255}));
    end SMIB_Partial;
  end BaseNetwork;

  package OutputInterfaces
    extends Modelica.Icons.InterfacesPackage;
    partial model OutputsInterface
    public
      Modelica.Blocks.Interfaces.RealOutput Vt
        annotation (Placement(transformation(extent={{140,70},{160,90}}),
            iconTransformation(extent={{140,70},{160,90}})));
      Modelica.Blocks.Interfaces.RealOutput Q
        annotation (Placement(transformation(extent={{140,-10},{160,10}}),
            iconTransformation(extent={{140,-10},{160,10}})));
      Modelica.Blocks.Interfaces.RealOutput P
        annotation (Placement(transformation(extent={{140,30},{160,50}}),
            iconTransformation(extent={{140,30},{160,50}})));
      Modelica.Blocks.Interfaces.RealOutput w
        annotation (Placement(transformation(extent={{140,-50},{160,-30}}),
            iconTransformation(extent={{140,-50},{160,-30}})));
      Modelica.Blocks.Interfaces.RealOutput delta
        annotation (Placement(transformation(extent={{140,-90},{160,-70}}),
            iconTransformation(extent={{140,-90},{160,-70}})));
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
</html>"),
        Diagram(coordinateSystem(extent={{-140,-140},{140,140}})),
        Icon(coordinateSystem(extent={{-140,-140},{140,140}})));
    end OutputsInterface;

    partial model OutputsInterfaceBig
    public
      Modelica.Blocks.Interfaces.RealOutput Vt
        annotation (Placement(transformation(extent={{200,70},{220,90}}),
            iconTransformation(extent={{200,70},{220,90}})));
      Modelica.Blocks.Interfaces.RealOutput Q
        annotation (Placement(transformation(extent={{200,-10},{220,10}}),
            iconTransformation(extent={{200,-10},{220,10}})));
      Modelica.Blocks.Interfaces.RealOutput P
        annotation (Placement(transformation(extent={{200,30},{220,50}}),
            iconTransformation(extent={{200,30},{220,50}})));
      Modelica.Blocks.Interfaces.RealOutput w
        annotation (Placement(transformation(extent={{200,-50},{220,-30}}),
            iconTransformation(extent={{200,-50},{220,-30}})));
      Modelica.Blocks.Interfaces.RealOutput delta
        annotation (Placement(transformation(extent={{200,-90},{220,-70}}),
            iconTransformation(extent={{200,-90},{220,-70}})));
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
</html>"),
        Diagram(coordinateSystem(extent={{-200,-140},{200,140}})),
        Icon(coordinateSystem(extent={{-200,-140},{200,140}})));
    end OutputsInterfaceBig;

    partial model OutputsInterfaceBusVoltagesBranchPowers_noInfiniteBus
      Modelica.Blocks.Interfaces.RealOutput Pline1 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-150,-190}),
                               iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-20,-210})));
      Modelica.Blocks.Interfaces.RealOutput Qline1 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-130,-190}),
                               iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={60,-210})));
      Modelica.Blocks.Interfaces.RealOutput Pline2 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-100,-190}),
                              iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={0,-210})));
      Modelica.Blocks.Interfaces.RealOutput Qline2 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-80,-190}),
                              iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={80,-210})));
      Modelica.Blocks.Interfaces.RealOutput Pline3 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-40,-190}),iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={20,-210})));
      Modelica.Blocks.Interfaces.RealOutput Qline3 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-20,-190}),iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={100,-210})));
      Modelica.Blocks.Interfaces.RealOutput Pline4 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={20,-190}),iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={40,-210})));
      Modelica.Blocks.Interfaces.RealOutput Qline4 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={40,-190}),iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={120,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bvm1 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-150,-150}),
                              iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-180,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bva1 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-130,-150}),
                              iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-100,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bvm2 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-90,-150}),
                             iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-160,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bva2 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-70,-150}),
                             iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-80,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bvm4 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-20,-150}),
                              iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-140,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bva4 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={0,-150}), iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={-60,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bvadiff1to2 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={80,-150}),  iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={140,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bvadiff1to3 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={102,-150}), iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={160,-210})));
      Modelica.Blocks.Interfaces.RealOutput Bvadiff1to4 annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={122,-150}), iconTransformation(
            extent={{-10,-10},{10,10}},
            rotation=270,
            origin={180,-210})));
    equation
    //   // Assignment of outputs for the network voltages and powers
    //   Bvm1 = B1.V;
    //   Bva1 = B1.angle;
    //   Bvm2 = B2.V;
    //   Bva2 = B2.angle;
    //   Bvm3 = B3.V;
    //   Bva3 = B3.angle;
    //   Bvm4 = B4.V;
    //   Bva4 = B4.angle;
    //   Pline1 = line_1.P12;
    //   Qline1 = line_1.Q12;
    //   Pline2 = line_2.P12;
    //   Qline2 = line_2.Q12;
    //   Pline3 = line_3.P12;
    //   Qline3 = line_3.Q12;
    //   Pline4 = line_4.P12;
    //   Qline4 = line_4.Q12;
    //   Bvadiff1to2 = B1.angle - B2.angle;
    //   Bvadiff1to3 = B1.angle - B3.angle;
    //   Bvadiff1to4 = B1.angle - B4.angle;
      annotation (
        experiment(
          StopTime=10,
          Interval=0.0001,
          Tolerance=1e-06,
          __Dymola_fixedstepsize=0.0001,
          __Dymola_Algorithm="Dassl"),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>This interface allows to create outputs for the network variables such as voltages, powers and bus voltage angle differences.</p>
<p>When placed in a model extending from one of the networks in BaseNetwork, the following text should be added:</p>
<p>Note that in this example there is an additional line that has been added to the SMIBPS_IdControl.BaseModelsPartial.BaseNetwork.SMIB_Partial model, this is to show how the interface has to be extended from this level to add additional variables when the base model is extended with extra components.</p>
<p>See the example of usage under the interface: SMIBPS_IdControl.Analysis.ClassicalControlDesign.Interfaces.SMIB_AVR_wLineRmovaland4inputs</p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;//&nbsp;Assignment&nbsp;of&nbsp;outputs&nbsp;for&nbsp;the&nbsp;network&nbsp;voltages&nbsp;and&nbsp;powers</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvm1&nbsp;=&nbsp;B1.V;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bva1&nbsp;=&nbsp;B1.angle;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvm2&nbsp;=&nbsp;B2.V;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bva2&nbsp;=&nbsp;B2.angle;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvm3&nbsp;=&nbsp;B3.V;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bva3&nbsp;=&nbsp;B3.angle;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvm4&nbsp;=&nbsp;B4.V;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bva4&nbsp;=&nbsp;B4.angle;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Pline1&nbsp;=&nbsp;line_1.P12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Qline1&nbsp;=&nbsp;line_1.Q12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Pline2&nbsp;=&nbsp;line_2.P12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Qline2&nbsp;=&nbsp;line_2.Q12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Pline3&nbsp;=&nbsp;line_3.P12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Qline3&nbsp;=&nbsp;line_3.Q12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Pline4&nbsp;=&nbsp;line_4.P12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Qline4&nbsp;=&nbsp;line_4.Q12;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvadiff1to2&nbsp;=&nbsp;B1.angle&nbsp;-&nbsp;B2.angle;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvadiff1to3&nbsp;=&nbsp;B1.angle&nbsp;-&nbsp;B3.angle;</span></p>
<p><span style=\"font-family: Courier New; color: #006400;\">//&nbsp;&nbsp;&nbsp;Bvadiff1to4&nbsp;=&nbsp;B1.angle&nbsp;-&nbsp;B4.angle;</span></p>
</html>"),
        Diagram(coordinateSystem(extent={{-200,-200},{200,140}}),
                                                                graphics={Rectangle(
              extent={{-164,-174},{-110,-200}},
              lineColor={255,0,0},
              lineThickness=1,
              fillColor={255,170,213},
              fillPattern=FillPattern.Solid), Rectangle(
              extent={{-180,-132},{160,-200}},
              lineColor={28,108,200},
              lineThickness=1),
            Text(
              extent={{-142,-114},{118,-134}},
              lineColor={255,0,0},
              lineThickness=1,
              fillColor={255,170,213},
              fillPattern=FillPattern.None,
              textString="Assignment of Outputs is Should be Done in the Text Layer where this Interface is Placed
"),                                                                       Rectangle(
              extent={{10,-180},{64,-200}},
              lineColor={255,0,0},
              lineThickness=1,
              fillColor={255,170,213},
              fillPattern=FillPattern.Solid)}),
        Icon(coordinateSystem(extent={{-200,-200},{200,140}}),
                                                           graphics={
              Rectangle(
              extent={{-190,-180},{190,-200}},
              lineColor={28,108,200},
              lineThickness=1,
              fillColor={170,213,255},
              fillPattern=FillPattern.Solid), Text(
              extent={{-190,-195},{190,-205}},
              lineColor={0,0,255},
              lineThickness=1,
              fillColor={255,170,213},
              fillPattern=FillPattern.None,
              textString="Network Voltages, Line Powers and Bus Angle Differences

")}));
    end OutputsInterfaceBusVoltagesBranchPowers_noInfiniteBus;
  end OutputInterfaces;
end BaseModelsPartial;
