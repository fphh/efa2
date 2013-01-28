package modThreeWay_sto
  model RecA
    EFA_Blocks_simple.signalSource signalsource1(Offset = -1) annotation(Placement(visible = true, transformation(origin = {-89.899,36.0269}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.signalSource signalsource2(Rect_Period = 1, Rect_Amplitude = -1 / 0.9 / 0.8 - 1 / 0.9 / 0.8 / 0.8, Rect_StartTime = 0.25, Offset = 1 / 0.9 / 0.8) annotation(Placement(visible = true, transformation(origin = {-90.9091,8.08081}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon6 annotation(Placement(visible = true, transformation(origin = {-62.963,8.75421}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta3(eta = 0.8) annotation(Placement(visible = true, transformation(origin = {-38.0471,9.09091}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon5 annotation(Placement(visible = true, transformation(origin = {-11.7845,9.09091}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.w3_node w3_node1 annotation(Placement(visible = true, transformation(origin = {12.4579,26.936}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {36.7003,27.6094}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.4) annotation(Placement(visible = true, transformation(origin = {61.2795,27.2727}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon4 annotation(Placement(visible = true, transformation(origin = {87.5421,26.5993}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-61.9529,37.037}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.9) annotation(Placement(visible = true, transformation(origin = {-38.0471,37.3737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {-12.4579,37.037}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(powercon5.y,w3_node1.realinput1) annotation(Line(points = {{-1.71872,8.89354},{3.0303,8.89354},{3.0303,19.5825},{3.20538,19.5825}}));
    connect(eta3.y,powercon5.u) annotation(Line(points = {{-28.1787,8.53828},{-21.8855,8.53828},{-21.8855,8.81459},{-22.2056,8.81459}}));
    connect(powercon6.y,eta3.u) annotation(Line(points = {{-52.8972,8.55684},{-45.7912,8.55684},{-45.7912,8.89354},{-46.2182,8.89354}}));
    connect(signalsource2.y,powercon6.u) annotation(Line(points = {{-80.5275,9.02818},{-73.064,9.02818},{-73.064,8.47789},{-73.384,8.47789}}));
    connect(eta2.y,powercon4.u) annotation(Line(points = {{71.1479,26.7201},{77.7778,26.7201},{77.7778,26.323},{77.121,26.323}}));
    connect(powercon3.y,eta2.u) annotation(Line(points = {{46.7661,27.4121},{54.2088,27.4121},{54.2088,27.0754},{53.1084,27.0754}}));
    connect(w3_node1.realinput2,powercon3.u) annotation(Line(points = {{20.0135,27.2997},{25.9259,27.2997},{25.9259,27.3331},{26.2793,27.3331}}));
    connect(powercon2.y,w3_node1.u) annotation(Line(points = {{-2.39212,36.8397},{3.367,36.8397},{3.367,34.7744},{3.73064,34.7744}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-28.1787,36.8211},{-23.9057,36.8211},{-23.9057,36.7607},{-22.879,36.7607}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-51.8871,36.8397},{-45.7912,36.8397},{-45.7912,37.1764},{-46.2182,37.1764}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-79.5174,36.9743},{-72.3906,36.9743},{-72.3906,36.7607},{-72.3739,36.7607}}));
  end RecA;
  model RecB
    EFA_Blocks_simple.signalSource signalsource1(Offset = -1) annotation(Placement(visible = true, transformation(origin = {-89.899,36.0269}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.signalSource signalsource2(Rect_Period = 1, Rect_Amplitude = -1 / 0.9 / 0.8 - 1 / 0.9 / 0.8 / 0.8, Rect_StartTime = 0.25, Offset = 1 / 0.9 / 0.8) annotation(Placement(visible = true, transformation(origin = {-90.9091,8.08081}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon6 annotation(Placement(visible = true, transformation(origin = {-62.963,8.75421}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta3(eta = 0.85) annotation(Placement(visible = true, transformation(origin = {-38.0471,9.09091}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon5 annotation(Placement(visible = true, transformation(origin = {-11.7845,9.09091}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.w3_node w3_node1 annotation(Placement(visible = true, transformation(origin = {12.4579,26.936}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon3 annotation(Placement(visible = true, transformation(origin = {36.7003,27.6094}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta2(eta = 0.45) annotation(Placement(visible = true, transformation(origin = {61.2795,27.2727}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon4 annotation(Placement(visible = true, transformation(origin = {87.5421,26.5993}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon1 annotation(Placement(visible = true, transformation(origin = {-61.9529,37.037}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.ETA eta1(eta = 0.9) annotation(Placement(visible = true, transformation(origin = {-38.0471,37.3737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
    EFA_Blocks_simple.powerCon powercon2 annotation(Placement(visible = true, transformation(origin = {-12.4579,37.037}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  equation
    connect(powercon5.y,w3_node1.realinput1) annotation(Line(points = {{-1.71872,8.89354},{3.0303,8.89354},{3.0303,19.5825},{3.20538,19.5825}}));
    connect(eta3.y,powercon5.u) annotation(Line(points = {{-28.1787,8.53828},{-21.8855,8.53828},{-21.8855,8.81459},{-22.2056,8.81459}}));
    connect(powercon6.y,eta3.u) annotation(Line(points = {{-52.8972,8.55684},{-45.7912,8.55684},{-45.7912,8.89354},{-46.2182,8.89354}}));
    connect(signalsource2.y,powercon6.u) annotation(Line(points = {{-80.5275,9.02818},{-73.064,9.02818},{-73.064,8.47789},{-73.384,8.47789}}));
    connect(eta2.y,powercon4.u) annotation(Line(points = {{71.1479,26.7201},{77.7778,26.7201},{77.7778,26.323},{77.121,26.323}}));
    connect(powercon3.y,eta2.u) annotation(Line(points = {{46.7661,27.4121},{54.2088,27.4121},{54.2088,27.0754},{53.1084,27.0754}}));
    connect(w3_node1.realinput2,powercon3.u) annotation(Line(points = {{20.0135,27.2997},{25.9259,27.2997},{25.9259,27.3331},{26.2793,27.3331}}));
    connect(powercon2.y,w3_node1.u) annotation(Line(points = {{-2.39212,36.8397},{3.367,36.8397},{3.367,34.7744},{3.73064,34.7744}}));
    connect(eta1.y,powercon2.u) annotation(Line(points = {{-28.1787,36.8211},{-23.9057,36.8211},{-23.9057,36.7607},{-22.879,36.7607}}));
    connect(powercon1.y,eta1.u) annotation(Line(points = {{-51.8871,36.8397},{-45.7912,36.8397},{-45.7912,37.1764},{-46.2182,37.1764}}));
    connect(signalsource1.y,powercon1.u) annotation(Line(points = {{-79.5174,36.9743},{-72.3906,36.9743},{-72.3906,36.7607},{-72.3739,36.7607}}));
  end RecB;
end modThreeWay_sto;

