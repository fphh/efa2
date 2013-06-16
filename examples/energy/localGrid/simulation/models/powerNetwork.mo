model powerNetwork
  GHSimulation.Grid.Transformer transformer1(powerScale = 10 ^ 6, etaMax = 0.9, filename = "maps/eta.txt", tablename = "transformer") annotation(Placement(visible = true, transformation(origin = {5.4645,15.5737}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.StoragePlant storageplant1(filename = "maps/eta.txt", tablename = "storage", powerScale = 10 ^ 6, etaMax = 0.6) annotation(Placement(visible = true, transformation(origin = {-39.8907,-7.65019}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.GasPowerPlant gaspowerplant1(filename = "maps/eta.txt", tablename = "gas", powerScale = 1.0, maxEta = 0.4) annotation(Placement(visible = true, transformation(origin = {-39.3443,38.7978}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.Wind wind1(filenamePower = "maps/power.txt", tablenamePower = "wind", powerScalePower = 1 * 10 ^ 6, filenameEta = "maps/eta.txt", tablenameEta = "wind", powerScaleEta = 1 * 10 ^ 6, maxEta = 0.4) annotation(Placement(visible = true, transformation(origin = {-41.8032,-55.4645}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  Modelica.Blocks.Sources.Constant const(k = 1 * 10 ^ 6) annotation(Placement(visible = true, transformation(origin = {-72.1311,80.6011}, extent = {{-5.08917,-5.08917},{5.08917,5.08917}}, rotation = 0)));
  GHSimulation.Grid.Industry industry1(filename = "maps/power.txt", tablename = "industry", powerScale = 0.5 * 10 ^ 6) annotation(Placement(visible = true, transformation(origin = {63.1525,-22.9508}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.HouseHold household1(filename = "maps/power.txt", tablename = "house", powerScale = 5 * 10 ^ 6) annotation(Placement(visible = true, transformation(origin = {62.5306,33.0601}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.Solar solar1(filenamePower = "maps/power.txt", tablenamePower = "solar", powerScalePower = 3 * 10 ^ 6, filenameEta = "maps/eta.txt", tablenameEta = "solar", powerScaleEta = 3 * 10 ^ 6, maxEta = 0.7) annotation(Placement(visible = true, transformation(origin = {9.05416,72.1312}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  GHSimulation.Grid.CoalPowerPlant coalpowerplant1(filename = "maps/eta.txt", tablename = "coal", powerScale = 1.0, maxEta = 0.4) annotation(Placement(visible = true, transformation(origin = {-38.8356,78.6508}, extent = {{-12,-12},{12,12}}, rotation = 0)));
  annotation(experiment(StartTime = 0.0, StopTime = 23.0, Tolerance = 0.000001));
equation
  connect(transformer1.pin_n,industry1.pin_p) annotation(Line(points = {{14.6645,16.307},{31.0664,16.307},{31.0664,-18.3515},{54.6525,-18.3515},{54.6525,-18.6175}}));
  connect(storageplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-32.8574,-1.08353},{-12.677,-1.08353},{-12.677,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(wind1.pin_p,transformer1.pin_p) annotation(Line(points = {{-34.8659,-53.1312},{-12.677,-53.1312},{-12.677,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(const.y,coalpowerplant1.u) annotation(Line(points = {{-66.533,80.6011},{-48.2115,80.6011},{-48.2115,79.8174},{-48.469,79.8174}}));
  connect(coalpowerplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-31.9356,81.0169},{-12.4417,81.0169},{-12.4417,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
  connect(solar1.pin_p,transformer1.pin_n) annotation(Line(points = {{15.9542,74.4645},{31.1042,74.4645},{31.1042,16.1742},{14.6645,16.1742},{14.6645,16.307}}));
  connect(transformer1.pin_n,household1.pin_p) annotation(Line(points = {{14.6645,16.307},{31.1042,16.307},{31.1042,37.3934},{54.0306,37.3934}}));
  connect(gaspowerplant1.u,const.y) annotation(Line(points = {{-48.9776,39.9645},{-66.874,39.9645},{-66.874,80.6011},{-66.533,80.6011}}));
  connect(gaspowerplant1.pin_p,transformer1.pin_p) annotation(Line(points = {{-32.4443,41.1311},{-12.4417,41.1311},{-12.4417,16.7963},{-4.26883,16.7963},{-4.26883,16.507}}));
end powerNetwork;

