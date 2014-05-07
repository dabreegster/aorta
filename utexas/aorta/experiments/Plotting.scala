// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.experiments

import scala.collection.JavaConverters.seqAsJavaListConverter
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{JFreeChart, ChartFactory, ChartFrame}
import org.jfree.data.statistics.{HistogramDataset, DefaultBoxAndWhiskerCategoryDataset, Statistics}
import org.jfree.chart.plot.PlotOrientation

import utexas.aorta.common.Util

// TODO auto-titling and saving plots with good filenames

// Used for showing histograms or boxplots from a bunch of individual values
case class DistributionData(
  values_per_mode: Map[String, Array[Double]], title: String, label: String
) {
  def cap(limit: Double) = copy(values_per_mode = values_per_mode.mapValues(
    ls => ls.filter(_ < limit)
  ))
  def select(mode: String) = copy(values_per_mode = Map(mode -> values_per_mode(mode)))
  def vs_baseline() = copy(values_per_mode = (values_per_mode - "baseline").mapValues(agents =>
    agents.zip(values_per_mode("baseline")).map(_ match { case (x, baseline) => x - baseline })
  ))
}
// Histograms and boxplots from pre-binned values. Arrays of (bin, count)
case class PreBinnedData(
  bins_per_mode: Map[String, Array[(Double, Double)]], title: String, label: String
)
// (X, Y) pairs for association
case class ScatterData(
  points_per_mode: Map[String, Array[(Double, Double)]], title: String, x: String, y: String
) {
  // TODO mod title in cap, vs_baseline, etc
  def cap(limit: Double) = copy(points_per_mode = points_per_mode.mapValues(
    ls => ls.filter(pt => pt._2 < limit)
  ))
  def select(mode: String) = copy(points_per_mode = Map(mode -> points_per_mode(mode)))
  // Assumes higher is worse, so it does baseline - each mode. In the result, more positive is
  // better.
  def vs_baseline(base: String) = copy(
    points_per_mode = (points_per_mode - base).mapValues(agents => agents.zip(points_per_mode(base))
      .map(_ match { case (pt, baseline) => (pt._1, baseline._2 - pt._2) })
    ),
    title = s"$title (relative to $base)"
  )
}

trait PlotUtil {
  private val num_bins = 20

  private def add_xy(chart: XYSeriesCollection, name: String, data: Array[(Double, Double)]) {
    val series = new XYSeries(name)
    for ((x, y) <- data) {
      series.add(x, y)
    }
    chart.addSeries(series)
  }

  def scatterplot(data: ScatterData): JFreeChart = {
    val chart = new XYSeriesCollection()
    Util.log(s"Correlation between ${data.x} and ${data.y}")
    for ((mode, points) <- data.points_per_mode) {
      add_xy(chart, mode, points)
      val coefficient = Statistics.getCorrelation(
        points.map(_._1.asInstanceOf[java.lang.Number]),
        points.map(_._2.asInstanceOf[java.lang.Number])
      )
      Util.log(s"  $mode: r = $coefficient")
    }
    return ChartFactory.createScatterPlot(
      data.title, data.x, data.y, chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  // TODO hollow style would rock
  def histogram(data: DistributionData): JFreeChart = {
    val chart = new HistogramDataset()
    for ((mode, values) <- data.values_per_mode) {
      chart.addSeries(mode, values, num_bins)
    }
    return ChartFactory.createHistogram(
      data.title, data.label, "Count", chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  def histogram(data: PreBinnedData): JFreeChart = {
    val chart = new XYSeriesCollection()
    for ((mode, bin_counts) <- data.bins_per_mode) {
      add_xy(chart, mode, bin_counts)
    }
    return ChartFactory.createHistogram(
      data.title, data.label, "Count", chart, PlotOrientation.VERTICAL, true, false, false
    )
  }

  def boxplot(data: DistributionData): JFreeChart = {
    val chart = new DefaultBoxAndWhiskerCategoryDataset()
    for ((mode, values) <- data.values_per_mode) {
      chart.add(values.toList.asJava, mode, "")
    }
    return ChartFactory.createBoxAndWhiskerChart(data.title, "Mode", data.label, chart, true)
  }

  def show(chart: JFreeChart) {
    val frame = new ChartFrame("A nefarious plot", chart)
    frame.pack()
    frame.setVisible(true)
  }
}
