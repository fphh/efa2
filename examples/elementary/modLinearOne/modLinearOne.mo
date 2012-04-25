package modLinearOne
  model RectA
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-39.3939,17.1717}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.9) annotation(Placement(visible = true, transformation(origin = {-13.468,16.835}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {14.1414,16.4983}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.signalSource signalsource1(Rect_Amplitude = 1, Offset = -0.2, Rect_StartTime = 0.25) annotation(Placement(visible = true, transformation(origin = {-66.33,16.1616}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    annotation(experiment(StartTime = 0.0, StopTime = 1, Tolerance = 0.000001));
  equation
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-3.5996,16.2824},{4.0404,16.2824},{4.0404,16.222},{3.72036,16.222}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-29.3281,16.9743},{-21.2121,16.9743},{-21.2121,16.6376},{-21.6391,16.6376}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-55.9484,17.109},{-49.8316,17.109},{-49.8316,16.8954},{-49.815,16.8954}}));
  end RectA;
  annotation(experiment(StartTime = 0.0, StopTime = 2.0, Tolerance = 0.000001));
end modLinearOne;

