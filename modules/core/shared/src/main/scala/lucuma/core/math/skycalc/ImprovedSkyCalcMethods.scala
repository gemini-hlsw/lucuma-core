// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import lucuma.core.math.Constants._
import lucuma.core.math.JulianDate.J2000
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.annotation.unused
import spire.std.double._

trait ImprovedSkyCalcMethods {

  // defined quantities for apparent place transforms ..
  protected val XFORM_FROMSTD      = 1
  protected val XFORM_TOSTDEP: Int = -1
  protected val XFORM_JUSTPRE      = 1
  protected val XFORM_DOAPPAR      = 0

  // Constants needed in etcorr method
  protected val DELTS: Array[Double] =
    Array[Double](-2.72, 3.86, 10.46, 17.20, 21.16, 23.62, 24.02, 23.93, 24.33, 26.77, 29.15, 31.07,
      33.15, 35.73, 40.18, 45.48, 50.54, 54.34, 56.86, 60.78, 62.97)

  final protected class DoubleRef(var d: Double) {
    def this() = {
      this(0.0)
    }

    override def toString: String = d.toString
  }

  /**
    * Return the LST time as a ZonedDateTime object for the given instant.
    */
  protected def getLst(lstHours: Double, instant: Instant): ZonedDateTime = {
    val zdt     = instant.atZone(ZoneOffset.UTC)
    val h       = zdt.getHour
    val nextDay = lstHours < h
    setHours(zdt, lstHours, nextDay)
  }

  private def setHours(
    zdt:     ZonedDateTime,
    hours:   Double,
    nextDay: Boolean
  ): ZonedDateTime = {
    val h      = hours.toInt
    val md     = (hours - h) * 60.0
    val min    = md.toInt
    val sd     = (md - min) * 60.0
    val sec    = sd.toInt
    val ns     = ((sd - sec) * NanosInSecond).toInt
    val newZDT = zdt.`with`(LocalTime.of(h, min, sec, ns))
    if (nextDay) newZDT.plusHours(24) else newZDT
  }

  /**
    * Return the airmass for the given altitude in degrees.
    */
  def getAirmass(alt: Double): Double = {
    val secz = secant_z(alt)
    if (secz >= 0.0)
      if (secz < 12.0) return true_airmass(secz)
      else if (secz <= 99.0) return secz
    0.0
  }

  /**
    * This takes the date (which contains the time), and the site parameters,
    * and prints out a banner giving the various dates and times; also
    * computes and returns various jd's, the sidereal time, and the epoch.
    * Returns negative number to signal error if date is out of range of
    * validity of algorithms, or if you specify a bad time during daylight-time
    * change; returns zero if successful.
    */
  protected def setup_time_place(
    instant:  Instant,
    longit:   Double,
    jdut:     DoubleRef,
    sid:      DoubleRef,
    curepoch: DoubleRef
  ): Short = {
    val jd = instant_to_jd(instant)
    sid.d = lst(jd, longit)
    jdut.d = jd
    curepoch.d = 2000.0 + (jd - J2000.toDouble) / 365.25
    0
  }

  protected def cooxform(
    rin:          Double,
    din:          Double,
    std_epoch:    Double,
    date_epoch:   Double,
    rout:         DoubleRef,
    dout:         DoubleRef,
    just_precess: Int,
    from_std:     Int
  ): Unit = {
    /* all the 3-d stuff is declared as [4] 'cause I'm not using the
             zeroth element. */
    var ti         = .0
    var tf         = .0
    var zeta       = .0
    var z          = .0
    var theta      = .0
    /* all as per  Taff */
    var cosz       = .0
    var coszeta    = .0
    var costheta   = .0
    var sinz       = .0
    var sinzeta    = .0
    var sintheta   = .0
    /* ftns */
    val p          = Array.ofDim[Double](4, 4)
    /* elements of the rotation matrix */
    val n          = Array.ofDim[Double](4, 4)
    /* elements of the nutation matrix */
    val r          = Array.ofDim[Double](4, 4)
    /* their product */
    val t          = Array.ofDim[Double](4, 4)
    /* temporary matrix for inversion .... */
    var radian_ra  = .0
    var radian_dec = .0
    /* nutation angles in radians */
    val del_psi    = new DoubleRef
    val del_eps    = new DoubleRef
    var eps        = .0
    val orig       = new Array[Double](4)
    /* original unit vector */
    val fin        = new Array[Double](4)
    /* final unit vector */
    var i          = 0
    var j          = 0
    var k          = 0
    ti = (std_epoch - 2000.0) / 100.0
    tf = (date_epoch - 2000.0 - 100.0 * ti) / 100.0
    zeta =
      (2306.2181 + 1.39656 * ti + 0.000139 * ti * ti) * tf + (0.30188 - 0.000344 * ti) * tf * tf + 0.017998 * tf * tf * tf
    z = zeta + (0.79280 + 0.000410 * ti) * tf * tf + 0.000205 * tf * tf * tf
    theta =
      (2004.3109 - 0.8533 * ti - 0.000217 * ti * ti) * tf - (0.42665 + 0.000217 * ti) * tf * tf - 0.041833 * tf * tf * tf
    /* convert to radians */
    zeta = zeta / ArcsecsInRadian
    z = z / ArcsecsInRadian
    theta = theta / ArcsecsInRadian
    /* compute the necessary trig functions for speed and simplicity */
    cosz = Math.cos(z)
    coszeta = Math.cos(zeta)
    costheta = Math.cos(theta)
    sinz = Math.sin(z)
    sinzeta = Math.sin(zeta)
    sintheta = Math.sin(theta)
    /* compute the elements of the precession matrix -- set up
               here as *from* standard epoch *to* input jd. */
    p(1)(1) = coszeta * cosz * costheta - sinzeta * sinz
    p(1)(2) = -1.0 * sinzeta * cosz * costheta - coszeta * sinz
    p(1)(3) = -1.0 * cosz * sintheta
    p(2)(1) = coszeta * sinz * costheta + sinzeta * cosz
    p(2)(2) = -1.0 * sinzeta * sinz * costheta + coszeta * cosz
    p(2)(3) = -1.0 * sinz * sintheta
    p(3)(1) = coszeta * sintheta
    p(3)(2) = -1.0 * sinzeta * sintheta
    p(3)(3) = costheta
    if (just_precess == XFORM_DOAPPAR) {
      /* if apparent place called for */ /* do the same for the nutation matrix. */
      nutation_params(date_epoch, del_psi, del_eps)
      eps = 0.409105 /* rough obliquity of ecliptic in radians */
      n(1)(1) = 1.0
      n(2)(2) = 1.0
      n(3)(3) = 1.0
      n(1)(2) = -1.0 * del_psi.d * Math.cos(eps)
      n(1)(3) = -1.0 * del_psi.d * Math.sin(eps)
      n(2)(1) = -1.0 * n(1)(2)
      n(2)(3) = -1.0 * del_eps.d
      n(3)(1) = -1.0 * n(1)(3)
      n(3)(2) = -1.0 * n(2)(3)
      /* form product of precession and nutation matrices ... */
      i = 1
      while (i <= 3) {
        j = 1
        while (j <= 3) {
          r(i)(j) = 0.0
          k = 1
          while (k <= 3) {
            r(i)(j) += p(i)(k) * n(k)(j)
            k += 1
          }
          j += 1
        }
        i += 1
      }
    } else {
      /* if you're just precessing .... */
      i = 1
      while (i <= 3) {
        j = 1
        while (j <= 3) {
          r(i)(j) = p(i)(j) /* simply copy precession matrix */
          j += 1
        }
        i += 1
      }
    }
    /* The inverse of a rotation matrix is its transpose ... */
    if (from_std == XFORM_TOSTDEP) {
      /* if you're transforming back to std
                              epoch, rather than forward from std */
      i = 1
      while (i <= 3) {
        j = 1
        while (j <= 3) {
          t(i)(j) = r(j)(i) /* store transpose ... */
          j += 1
        }
        i += 1
      }
      i = 1
      while (i <= 3) {
        j = 1
        while (j <= 3) {
          r(i)(j) = t(i)(j) /* replace original w/ transpose.*/
          j += 1
        }
        i += 1
      }
    }
    /* finally, transform original coordinates */
    radian_ra = rin / HrsInRadian
    radian_dec = din / DegsInRadian
    orig(1) = Math.cos(radian_dec) * Math.cos(radian_ra)
    orig(2) = Math.cos(radian_dec) * Math.sin(radian_ra)
    orig(3) = Math.sin(radian_dec)
    if (from_std == XFORM_TOSTDEP && just_precess == XFORM_DOAPPAR)
      /* if you're transforming from jd to std epoch, and doing apparent place,
            first step is to de-aberrate while still in epoch of date ... */
      aberrate(date_epoch, orig, from_std)
    i = 1
    while (i <= 3) {
      fin(i) = 0.0
      j = 1
      while (j <= 3) {
        fin(i) += r(i)(j) * orig(j)
        j += 1
      }
      i += 1
    }
    if (from_std == XFORM_FROMSTD && just_precess == XFORM_DOAPPAR)
      /* if you're transforming from std epoch to jd,
                  last step is to apply aberration correction once you're in
                  equinox of that jd. */
      aberrate(date_epoch, fin, from_std)
    /* convert back to spherical polar coords */
    xyz_cel(fin(1), fin(2), fin(3), rout, dout)
  }

  /**
    * computes the nutation parameters delta psi and
    * delta epsilon at julian epoch (in years) using approximate
    * formulae given by Jean Meeus, Astronomical Formulae for
    * Calculators, Willman-Bell, 1985, pp. 69-70. Accuracy
    * appears to be a few hundredths of an arcsec or better
    * and numerics have been checked against his example.
    * Nutation parameters are returned in radians.
    */
  protected def nutation_params(
    date_epoch: Double,
    del_psi:    DoubleRef,
    del_ep:     DoubleRef
  ): Unit = {
    var T      = .0
    var jd     = .0
    var L      = .0
    var Lprime = .0
    var M      = .0
    var Mprime = .0
    var Omega  = .0
    jd = (date_epoch - 2000.0) * 365.25 + J2000.toDouble
    T = (jd - 2415020.0) / 36525.0
    L = 279.6967 + (36000.7689 + 0.000303 * T) * T
    Lprime = 270.4342 + (481267.8831 - 0.001133 * T) * T
    M = 358.4758 + (35999.0498 - 0.000150 * T) * T
    Mprime = 296.1046 + (477198.8491 + 0.009192 * T) * T
    Omega = 259.1833 - (1934.1420 - 0.002078 * T) * T
    L = L / DegsInRadian
    Lprime = Lprime / DegsInRadian
    M = M / DegsInRadian
    Mprime = Mprime / DegsInRadian
    Omega = Omega / DegsInRadian
    del_psi.d = -1.0 * (17.2327 + 0.01737 * T) * Math.sin(
      Omega
    ) - (1.2729 + 0.00013 * T) * Math.sin(2.0 * L) + 0.2088 * Math.sin(
      2 * Omega
    ) - 0.2037 * Math.sin(2 * Lprime) + (0.1261 - 0.00031 * T) * Math.sin(
      M
    ) + 0.0675 * Math.sin(Mprime) - (0.0497 - 0.00012 * T) * Math.sin(
      2 * L + M
    ) - 0.0342 * Math.sin(2 * Lprime - Omega) - 0.0261 * Math.sin(
      2 * Lprime + Mprime
    ) + 0.0214 * Math.sin(2 * L - M) - 0.0149 * Math.sin(
      2 * L - 2 * Lprime + Mprime
    ) + 0.0124 * Math.sin(2 * L - Omega) + 0.0114 * Math.sin(
      2 * Lprime - Mprime
    )
    del_ep.d =
      (9.2100 + 0.00091 * T) * Math.cos(Omega) + (0.5522 - 0.00029 * T) * Math
        .cos(2 * L) - 0.0904 * Math.cos(2 * Omega) + 0.0884 * Math.cos(
        2.0 * Lprime
      ) + 0.0216 * Math.cos(2 * L + M) + 0.0183 * Math.cos(
        2 * Lprime - Omega
      ) + 0.0113 * Math.cos(2 * Lprime + Mprime) - 0.0093 * Math.cos(
        2 * L - M
      ) - 0.0066 * Math.cos(2 * L - Omega)
    del_psi.d = del_psi.d / ArcsecsInRadian
    del_ep.d = del_ep.d / ArcsecsInRadian
  }

  /**
    * A much cleaner rewrite of the original skycalc code for this,
    * which was transcribed from a PL/I routine ....
    */
  protected def xyz_cel(
    x0:  Double,
    y0:  Double,
    z0:  Double,
    ra:  DoubleRef,
    dec: DoubleRef
  ): Unit = {
    var x = x0
    var y = y0
    var z = z0

    /* corresponding right ascension and declination,
                                             returned in decimal hours and decimal degrees. */
    var mod = .0
    /* modulus */
    var xy  = .0 /* component in xy plane */
    /* normalize explicitly and check for bad input */
    mod = Math.sqrt(x * x + y * y + z * z)
    if (mod > 0.0) {
      x = x / mod
      y = y / mod
      z = z / mod
    } else {
      /* this has never happened ... */
      System.out.println(
        "Bad data in xyz_cel .... zero modulus position vector.\n"
      )
      ra.d = 0.0
      dec.d = 0.0
      return
    }
    xy = Math.sqrt(x * x + y * y)
    if (xy < 1.0e-11) {
      /* practically on a pole -- limit is arbitrary ...  */
      ra.d = 0.0 /* degenerate anyway */
      dec.d = HalfPi
      if (z < 0.0) dec.d *= -1.0
    } else {
      /* in a normal part of the sky ... */
      dec.d = Math.asin(z)
      ra.d = atan_circ(x, y)
    }
    ra.d *= HrsInRadian
    dec.d *= DegsInRadian
  }

  /**
    * corrects celestial unit vector for aberration due to earth's motion.
    * Uses accurate sun position ... replace with crude one for more speed if
    * needed.
    */
  protected def aberrate(
    epoch:    Double,
    vec:      Array[Double],
    from_std: Int
  ): Unit = {
    /* 1 = apply aberration, -1 = take aberration out. */
    var jd      = .0
    var jd1     = .0
    var jd2     = .0
    var Xdot    = .0
    var Ydot    = .0
    var Zdot    = .0
    /* page C24 */
    /* throwaways */
    val ras     = new DoubleRef
    val decs    = new DoubleRef
    val dists   = new DoubleRef
    val topora  = new DoubleRef
    val topodec = new DoubleRef
    val x       = new DoubleRef
    val y       = new DoubleRef
    val z       = new DoubleRef
    val x1      = new DoubleRef
    val y1      = new DoubleRef
    val z1      = new DoubleRef
    val x2      = new DoubleRef
    val y2      = new DoubleRef
    val z2      = new DoubleRef
    var norm    = .0
    /* find heliocentric velocity of earth as a fraction of the speed of light ... */
    jd = J2000.toDouble + (epoch - 2000.0) * 365.25
    jd1 = jd - EarthDiff
    jd2 = jd + EarthDiff
    accusun(jd1, 0.0, 0.0, ras, decs, dists, topora, topodec, x1, y1, z1)
    accusun(jd2, 0.0, 0.0, ras, decs, dists, topora, topodec, x2, y2, z2)
    accusun(jd, 0.0, 0.0, ras, decs, dists, topora, topodec, x, y, z)
    Xdot =
      MetersPerSecondInAUPerDay * (x2.d - x1.d) / (2.0 * EarthDiff * SpeedOfLight.value) /* numerical differentiation */
    Ydot =
      MetersPerSecondInAUPerDay * (y2.d - y1.d) / (2.0 * EarthDiff * SpeedOfLight.value) /* crude but accurate */
    Zdot = MetersPerSecondInAUPerDay * (z2.d - z1.d) / (2.0 * EarthDiff * SpeedOfLight.value)
    /* approximate correction ... non-relativistic but very close.  */
    vec(1) += from_std * Xdot
    vec(2) += from_std * Ydot
    vec(3) += from_std * Zdot
    norm = Math.pow(vec(1) * vec(1) + vec(2) * vec(2) + vec(3) * vec(3), 0.5)
    vec(1) = vec(1) / norm
    vec(2) = vec(2) / norm
    vec(3) = vec(3) / norm
  }

  /**
    * returns radian angle 0 to 2pi for coords x, y --
    * get that quadrant right !!
    */
  protected def atan_circ(x: Double, y: Double): Double = {
    var theta = .0
    if ((x == 0.0) && (y == 0.0)) return 0.0 /* guard ... */
    theta = Math.atan2(y, x)
    while (theta < 0.0) theta += TwoPi
    theta
  }

  /**
    * implemenataion of Jean Meeus' more accurate solar
    * ephemeris.  For ultimate use in helio correction! From
    * Astronomical Formulae for Calculators, pp. 79 ff.  This
    * gives sun's position wrt *mean* equinox of date, not
    * apparent*.  Accuracy is << 1 arcmin.  Positions given are
    * geocentric ... parallax due to observer's position on earth is
    * ignored. This is up to 8 arcsec; routine is usually a little
    * better than that.
    * // -- topocentric correction *is* included now. -- //
    * Light travel time is apparently taken into
    * account for the ra and dec, but I don't know if aberration is
    * and I don't know if distance is simlarly antedated.
    * <p/>
    * x, y, and z are heliocentric equatorial coordinates of the
    * EARTH, referred to mean equator and equinox of date.
    */
  protected def accusun(
    jd0:     Double,
    lst:     Double,
    geolat:  Double,
    ra:      DoubleRef,
    dec:     DoubleRef,
    dist:    DoubleRef,
    topora:  DoubleRef,
    topodec: DoubleRef,
    x:       DoubleRef,
    y:       DoubleRef,
    z:       DoubleRef
  ): Unit = {
    var jd       = jd0
    var L        = .0
    var T        = .0
    var Tsq      = .0
    var Tcb      = .0
    var M        = .0
    var e        = .0
    var Cent     = .0
    var nu       = .0
    var sunlong  = .0
    var Mrad     = .0
    var nurad    = .0
    var R        = .0
    var A        = .0
    var B        = .0
    var C        = .0
    var D        = .0
    var E        = .0
    var H        = .0
    var xtop     = .0
    var ytop     = .0
    var ztop     = .0
    var topodist = .0
    var l        = .0
    var m        = .0
    var n        = .0
    val xgeo     = new DoubleRef
    val ygeo     = new DoubleRef
    val zgeo     = new DoubleRef
    jd = jd + etcorr(jd) / SecsInDay /* might as well do it right .... */
    T = (jd - 2415020.0) / 36525.0 /* 1900 --- this is an oldish theory*/
    Tsq = T * T
    Tcb = T * Tsq
    L = 279.69668 + 36000.76892 * T + 0.0003025 * Tsq
    M = 358.47583 + 35999.04975 * T - 0.000150 * Tsq - 0.0000033 * Tcb
    e = 0.01675104 - 0.0000418 * T - 0.000000126 * Tsq
    L = circulo(L)
    M = circulo(M)
    /*      printf("raw L, M: %15.8f, %15.8f\n",L,M); */
    A = 153.23 + 22518.7541 * T /* A, B due to Venus */
    B = 216.57 + 45037.5082 * T
    C = 312.69 + 32964.3577 * T /* C due to Jupiter */
    /* D -- rough correction from earth-moon
                    barycenter to center of earth. */
    D = 350.74 + 445267.1142 * T - 0.00144 * Tsq
    E = 231.19 + 20.20 * T /* "inequality of long period .. */
    H = 353.40 + 65928.7155 * T /* Jupiter. */
    A = circulo(A) / DegsInRadian
    B = circulo(B) / DegsInRadian
    C = circulo(C) / DegsInRadian
    D = circulo(D) / DegsInRadian
    E = circulo(E) / DegsInRadian
    H = circulo(H) / DegsInRadian
    L = L + 0.00134 * Math.cos(A) + 0.00154 * Math.cos(B) + 0.00200 * Math.cos(
      C
    ) + 0.00179 * Math.sin(D) + 0.00178 * Math.sin(E)
    Mrad = M / DegsInRadian
    Cent = (1.919460 - 0.004789 * T - 0.000014 * Tsq) * Math.sin(
      Mrad
    ) + (0.020094 - 0.000100 * T) * Math.sin(2.0 * Mrad) + 0.000293 * Math.sin(
      3.0 * Mrad
    )
    sunlong = L + Cent
    nu = M + Cent
    nurad = nu / DegsInRadian
    R = (1.0000002 * (1.0 - e * e)) / (1.0 + e * Math.cos(nurad))
    R = R + 0.00000543 * Math.sin(A) + 0.00001575 * Math.sin(
      B
    ) + 0.00001627 * Math.sin(C) + 0.00003076 * Math.cos(D) + 0.00000927 * Math
      .sin(H)
    sunlong = sunlong / DegsInRadian
    dist.d = R
    x.d = Math.cos(sunlong) /* geocentric */
    y.d = Math.sin(sunlong)
    z.d = 0.0
    eclrot(jd, y, z)
    /*      --- code to include topocentric correction for sun .... */
    geocent(lst, geolat, 0.0, xgeo, ygeo, zgeo)
    xtop = x.d - xgeo.d * (EquatorialRadius[Double] / AstronomicalUnit[Double]).value
    ytop = y.d - ygeo.d * (EquatorialRadius[Double] / AstronomicalUnit[Double]).value
    ztop = z.d - zgeo.d * (EquatorialRadius[Double] / AstronomicalUnit[Double]).value
    topodist = Math.sqrt(xtop * xtop + ytop * ytop + ztop * ztop)
    l = xtop / topodist
    m = ytop / topodist
    n = ztop / topodist
    topora.d = atan_circ(l, m) * HrsInRadian
    topodec.d = Math.asin(n) * DegsInRadian
    ra.d = atan_circ(x.d, y.d) * HrsInRadian
    dec.d = Math.asin(z.d) * DegsInRadian
    x.d = x.d * R * -1 /* heliocentric */
    y.d = y.d * R * -1
    z.d = z.d * R * -1
  }

  /**
    * Given a julian date in 1900-2100, returns the correction
    * delta t which is:
    * TDT - UT (after 1983 and before 1998)
    * ET - UT (before 1983)
    * an extrapolated guess  (after 1998).
    * <p/>
    * For dates in the past (<= 1998 and after 1900) the value is linearly
    * interpolated on 5-year intervals; for dates after the present,
    * an extrapolation is used, because the true value of delta t
    * cannot be predicted precisely.  Note that TDT is essentially the
    * modern version of ephemeris time with a slightly cleaner
    * definition.
    * <p/>
    * Where the algorithm shifts there is an approximately 0.1 second
    * discontinuity.  Also, the 5-year linear interpolation scheme can
    * lead to errors as large as 0.5 seconds in some cases, though
    * usually rather smaller.
    */
  protected def etcorr(jd: Double): Double = {
    val dates = new Array[Double](22)
    var year  = .0
    var delt  = 0.0
    var i     = 0
    i = 0
    while (i <= 19) {
      dates(i) = 1900 + i * 5.0

      i += 1
    }
    dates(20) = 1998.0 /* the last accurately tabulated one in the
                                2000 Almanac ... */
    year = 1900.0 + (jd - 2415019.5) / 365.25
    if (year < 1998.0 && year >= 1900.0) {
      i = (year - 1900.0).toInt / 5
      delt = DELTS(i) + ((DELTS(i + 1) - DELTS(i)) / (dates(i + 1) - dates(
        i
      ))) * (year - dates(i))
    } else if (year >= 1998.0 && year < 2100.0)
      delt = 33.15 + 2.164e-3 * (jd - 2436935.4) /* rough extrapolation */
    else if (year < 1900.0) {
      System.out.println("etcorr ... no ephemeris time data for < 1900.\n")
      delt = 0.0
    } else if (year >= 2100.0) {
      System.out.println(
        "etcorr .. very long extrapolation in delta T - inaccurate.\n"
      )
      delt = 180.0 /* who knows? */
    }
    delt
  }

  /**
    * assuming x is an angle in degrees, returns
    * modulo 360 degrees.
    */
  protected def circulo(x: Double): Double = {
    val n = (x / 360.0).toInt
    x - 360.0 * n
  }

  /**
    * rotates ecliptic rectangular coords x, y, z to
    * equatorial (all assumed of date.)
    */
  protected def eclrot(jd: Double, y: DoubleRef, z: DoubleRef): Unit = {
    var incl = .0
    //        double xpr;
    var ypr  = .0
    var zpr  = .0
    var T    = .0
    T = (jd - J2000.toDouble) / 36525 /* centuries since J2000 */
    incl = (23.439291 + T * (-0.0130042 - 0.00000016 * T)) / DegsInRadian
    /* 1992 Astron Almanac, p. B18, dropping the
                   cubic term, which is 2 milli-arcsec! */
    ypr = Math.cos(incl) * y.d - Math.sin(incl) * z.d
    zpr = Math.sin(incl) * y.d + Math.cos(incl) * z.d
    y.d = ypr
    z.d = zpr
    /* x remains the same. */
  }

  protected def eclrot(
    jd:        Double,
    @unused x: DoubleRef,
    y:         DoubleRef,
    z:         DoubleRef
  ): Unit = {
    var incl          = .0
    var /*xpr, */ ypr = .0
    var zpr           = .0
    var T             = .0
    T = (jd - J2000.toDouble) / 36525
    incl = (23.439291 + T * (-0.0130042 - 0.00000016 * T)) / DegsInRadian
    ypr = Math.cos(incl) * y.d - Math.sin(incl) * z.d
    zpr = Math.sin(incl) * y.d + Math.cos(incl) * z.d
    y.d = ypr
    z.d = zpr
  }

  /**
    * computes the geocentric coordinates from the geodetic
    * (standard map-type) longitude, latitude, and height.
    * These are assumed to be in decimal hours, decimal degrees, and
    * meters respectively.  Notation generally follows 1992 Astr Almanac,
    * p. K11
    */
  protected def geocent(
    geolong0: Double,
    geolat0:  Double,
    height:   Double,
    x_geo:    DoubleRef,
    y_geo:    DoubleRef,
    z_geo:    DoubleRef
  ): Unit = {
    var geolong = geolong0
    var geolat  = geolat0
    var denom   = .0
    var C_geo   = .0
    var S_geo   = .0
    geolat = geolat / DegsInRadian
    geolong = geolong / HrsInRadian
    denom = (1.0 - FlatteningOfEarth) * Math.sin(geolat)
    denom = Math.cos(geolat) * Math.cos(geolat) + denom * denom
    C_geo = 1.0 / Math.sqrt(denom)
    S_geo = (1.0 - FlatteningOfEarth) * (1.0 - FlatteningOfEarth) * C_geo
    C_geo = C_geo + height / EquatorialRadius[Double].value /* deviation from almanac
                   notation -- include height here. */
    S_geo = S_geo + height / EquatorialRadius[Double].value
    x_geo.d = C_geo * Math.cos(geolat) * Math.cos(geolong)
    y_geo.d = C_geo * Math.cos(geolat) * Math.sin(geolong)
    z_geo.d = S_geo * Math.sin(geolat)
  }

  /**
    * adjusts a time (decimal hours) to be between -12 and 12,
    * generally used for hour angles.
    */
  protected def adj_time(x0: Double): Double = {
    var x = x0
    if (Math.abs(x) < 100000.0) {
      /* too inefficient for this! */
      while (x > 12.0) x = x - 24.0
      while (x < -12.0) x = x + 24.0
    } else System.out.println("Out of bounds in adj_time!\n")
    x
  }

  /**
    * returns altitude(degr) for dec, ha, lat (decimal degr, hr, degr);
    * also computes and returns azimuth through pointer argument,
    * and as an extra added bonus returns parallactic angle (decimal degr)
    * through another pointer argument.
    *
    * @param dec target declination in degrees
    * @param ha  the hour angle in hours
    * @param lat the observer's latitude in radians
    * @return the parallactic angle in degrees
    */
  protected def altit(
    dec0:   Double,
    ha0:    Double,
    lat0:   Double,
    az:     DoubleRef,
    parang: DoubleRef
  ): Double = {
    var dec    = dec0
    var ha     = ha0
    var lat    = lat0
    var x      = .0
    var y      = .0
    var z      = .0
    var sinp   = .0
    var cosp   = .0
    /* sin and cos of parallactic angle */
    var cosdec = .0
    var sindec = .0
    var cosha  = .0
    var sinha  = .0
    var coslat = .0
    var sinlat = .0
    /* time-savers ... */
    dec = dec / DegsInRadian
    ha = ha / HrsInRadian
    lat = lat / DegsInRadian /* thank heavens for pass-by-value */
    cosdec = Math.cos(dec)
    sindec = Math.sin(dec)
    cosha = Math.cos(ha)
    sinha = Math.sin(ha)
    coslat = Math.cos(lat)
    sinlat = Math.sin(lat)
    x = DegsInRadian * Math.asin(cosdec * cosha * coslat + sindec * sinlat)
    y = sindec * coslat - cosdec * cosha * sinlat /* due N comp. */
    z = -1.0 * cosdec * sinha /* due east comp. */
    az.d = Math.atan2(z, y)
    /* as it turns out, having knowledge of the altitude and
                   azimuth makes the spherical trig of the parallactic angle
                   less ambiguous ... so do it here!  Method uses the
               "astronomical triangle" connecting celestial pole, object,
                   and zenith ... now know all the other sides and angles,
                   so we can crush it ... */
    if (cosdec != 0.0) {
      /* protect divide by zero ... */
      sinp = -1.0 * Math.sin(az.d) * coslat / cosdec
      /* spherical law of sines .. note cosdec = sin of codec,
                      coslat = sin of colat .... */
      cosp = -1.0 * Math.cos(az.d) * cosha - Math.sin(az.d) * sinha * sinlat
      /* spherical law of cosines ... also transformed to local
                                available variables. */
      parang.d = Math.atan2(sinp, cosp) * DegsInRadian
      /* let the library function find the quadrant ... */
    } else /* you're on the pole */
    if (lat >= 0.0) parang.d = 180.0
    else parang.d = 0.0
    az.d *= DegsInRadian /* done with taking trig functions of it ... */
    while (az.d < 0.0) az.d += 360.0 /* force 0 -> 360 */
    while (az.d >= 360.0) az.d -= 360.0
    x
  }

  /**
    * Computes the secant of z, assuming the object is not
    * too low to the horizon; returns 100. if the object is
    * low but above the horizon, -100. if the object is just
    * below the horizon.
    */
  protected def secant_z(alt: Double): Double = {
    var secz = .0
    if (alt != 0) secz = 1.0 / Math.sin(alt / DegsInRadian)
    else secz = 100.0
    if (secz > 100.0) secz = 100.0
    if (secz < -100.0) secz = -100.0
    secz
  }

  /**
    * returns the true airmass for a given secant z.
    * The expression used is based on a tabulation of the mean KPNO
    * atmosphere given by C. M. Snell & A. M. Heiser, 1968,
    * PASP, 80, 336.  They tabulated the airmass at 5 degr
    * intervals from z = 60 to 85 degrees; I fit the data with
    * a fourth order poly for (secz - airmass) as a function of
    * (secz - 1) using the IRAF curfit routine, then adjusted the
    * zeroth order term to force (secz - airmass) to zero at
    * z = 0.  The poly fit is very close to the tabulated points
    * (largest difference is 3.2e-4) and appears smooth.
    * This 85-degree point is at secz = 11.47, so for secz > 12
    * I just return secz - 1.5 ... about the largest offset
    * properly determined.
    */
  protected def true_airmass(secz: Double): Double = {
    var seczmin1 = .0
    var i        = 0
    val ord      = 4
    val coef     = new Array[Double](5)
    var result   = 0.0
    coef(1) = 2.879465e-3 /* sun compilers do not allow automatic
                initializations of arrays. */
    coef(2) = 3.033104e-3
    coef(3) = 1.351167e-3
    coef(4) = -4.716679e-5
    if (secz < 0.0) return -1.0 /* out of range. */
    if (secz > 12) return secz - 1.5 /* shouldn't happen .... */
    seczmin1 = secz - 1.0
    /* evaluate polynomial ... */
    i = ord
    while (i > 0) {
      result = (result + coef(i)) * seczmin1

      i -= 1
    }
    /* no zeroth order term. */
    result = secz - result
    result
  }

  protected def instant_to_jd(instant: Instant): Double = {
    var yr1    = 0
    var mo1    = 1
    val jdzpt  = 1720982
    var jdint  = 0L
    var inter  = 0L
    var jd     = .0
    var jdfrac = .0
    val date   = instant.atZone(ZoneOffset.UTC)
    if (
      (date.getYear <= 1900) | (date.getYear >= 2100)
    ) //        printf("Date out of range.  1900 - 2100 only.\n");
      //        return(0.);
      throw new IllegalArgumentException(
        "Date out of range.  1900 - 2100 only."
      )
    if (date.getMonthValue <= 2) {
      yr1 = -1
      mo1 = 13
    }
    jdint = (365.25 * (date.getYear + yr1)).toLong /* truncates */
    inter = (30.6001 * (date.getMonthValue + mo1)).toLong
    jdint = jdint + inter + date.getDayOfMonth + jdzpt
    jd = jdint.toDouble
    jdfrac =
      date.getHour / 24.0 + date.getMinute / 1440.0 + (date.getSecond + date.getNano.toDouble / NanosInSecond) / SecsInDay
    if (jdfrac < 0.5) {
      jdint -= 1
      jdfrac = jdfrac + 0.5
    } else jdfrac = jdfrac - 0.5
    jd = jdint + jdfrac
    jd
  }

  /**
    * returns the local MEAN sidereal time (dec hrs) at julian date jd
    * at west longitude long (decimal hours).  Follows
    * definitions in 1992 Astronomical Almanac, pp. B7 and L2.
    * Expression for GMST at 0h ut referenced to Aoki et al, A&A 105,
    * p.359, 1982.  On workstations, accuracy (numerical only!)
    * is about a millisecond in the 1990s.
    */
  protected def lst(jd: Double, longit: Double): Double = {
    var t       = .0
    var ut      = .0
    var jdmid   = .0
    var jdint   = .0
    var jdfrac  = .0
    var sid_g   = .0
    var jdin    = 0L
    var sid_int = 0L
    jdin = jd.toLong /* fossil code from earlier package which
                                       split jd into integer and fractional parts ... */
    jdint = jdin.toDouble
    jdfrac = jd - jdint
    if (jdfrac < 0.5) {
      jdmid = jdint - 0.5
      ut = jdfrac + 0.5
    } else {
      jdmid = jdint + 0.5
      ut = jdfrac - 0.5
    }
    t = (jdmid - J2000.toDouble) / 36525
    sid_g =
      (24110.54841 + 8640184.812866 * t + 0.093104 * t * t - 6.2e-6 * t * t * t) / SecsInDay
    sid_int = sid_g.toLong
    sid_g = sid_g - sid_int
    sid_g = sid_g + 1.0027379093 * ut - longit / 24.0
    sid_int = sid_g.toLong
    sid_g = (sid_g - sid_int) * 24.0
    //        if (sid_g < 0.0) {
    //            sid_g = sid_g + 24.0;
    //        }
    sid_g
  }

  /**
    * @param mpa   moon phase angle in degrees
    * @param mdist moon/object distance in degreee
    * @param mZD   moon zenith distance [deg]
    * @param ZD    object zenith distance [deg]
    * @param sZD   Sun zenith distance [deg]
    */
  protected def sb(
    mpa:   Double,
    mdist: Double,
    mZD:   Double,
    ZD:    Double,
    sZD:   Double
  ): Double = {
    val degrad = 57.2957795130823d
    val k      = 0.172d      // ; mag/airmass for Hale Pohaku
    val a      = 2.51189d
    val Q      = 27.78151d
    val saltit = 90.0d - sZD // ; Sun's altitude
    //    ; Dark sky zenith V surface brightness
    //    Vzen = dblarr(n_elements(ZD))
    //    Vzen[*] = 21.587d
    var Vzen   = 21.587d
    //    ; Correct for brightening due to twilight
    //    ii = where(saltit gt -18.5)
    //    if (ii[0] ne -1) then Vzen[ii] = Vzen[ii] - ztwilight(saltit[ii])
    if (saltit > -18.5) Vzen -= ztwilight(saltit)
    //     Bzen = 0.263d *          a^(Q-Vzen)     ; zenith sky brightness
    val Bzen   = 0.263d * Math.pow(a, Q - Vzen)
    // ; sky constribution
    //     Bsky =Bzen*xair(ZD)*          10.^(-0.4d*k*(xair(ZD)-1.0d))
    val Bsky   = Bzen * xair(ZD) * Math.pow(10, -0.4d * k * (xair(ZD) - 1.0d))
    // ; moon contribution
    // n=n_elements(Bsky)
    //     istar=0.0d & fp=0.0d & Bmoon=dblarr(n)
    var istar  = 0.0d
    var fp     = 0.0d
    var Bmoon  = 0.0
    if (mZD <= 90.8) { //      istar=         10^ (-0.4d*(3.84d + 0.026d*abs(mpa) + (4.d-9)*          mpa^ 4))
      istar = Math.pow(
        10,
        -0.4d * (3.84d + 0.026d * Math.abs(mpa) + 4.0e-9 * Math.pow(mpa, 4))
      )
      if (
        mdist >= 10.0
      )                //        fp=(1.06d +          cos(mdist[j]/degrad)^ 2) *         10^ 5.36d  +          10^ (6.15d - mdist[j]/40.0d) $
        fp = (1.06d + Math.pow(Math.cos(mdist / degrad), 2)) * Math.pow(
          10,
          5.36d
        ) + Math.pow(10, 6.15d - mdist / 40.0d)
      else             //          fp=6.2d7/         mdist^ 2;
        fp = 6.2e7 / Math.pow(mdist, 2)
      //      Bmoon[j]=fp*istar*         10^ (-0.4d*k*xair(mZD[j]))*(1.0d -          10^ (-0.4d*k*xair(ZD[j])))
      Bmoon = fp * istar * Math.pow(10, -0.4d * k * xair(mZD)) * (1.0d - Math
        .pow(10, -0.4d * k * xair(ZD)))
    }
    //    ;print,istar,fp,Bmoon,Bsky
    //    ;print,Q-alog10((Bsky)/0.263)/alog10(a),Q-alog10((Bmoon)/0.263)/alog10(a)
    //    ; sky brightness in Vmag/arcsec^2
    //  return Q-    alog10((Bmoon+Bsky)/0.263)/    alog10(a);
    val ret    = Q - Math.log10((Bmoon + Bsky) / 0.263) / Math.log10(a)
    //    System.out.printf("sb(%1.2f, %1.2f, %1.2f, %1.2f, %1.2f) => %1.3f\n", mpa, mdist, mZD, ZD, sZD, ret);
    ret
  }

  protected def xair(z: Double): Double = { //    ;degrad=180.0d/!PI
    val degrad = 57.2957795130823d
    //  return,1.0d/sqrt(1.0d - 0.96d*         Math.sin(z/degrad)^ 2)
    1.0d / Math.sqrt(1.0d - 0.96d * Math.pow(Math.sin(z / degrad), 2))
  }

  protected def lunskybright(
    alpha0:    Double,
    rho:       Double,
    KZen:      Double,
    altmoon:   Double,
    alt:       Double,
    moondist0: Double
  ): Double = {
    var alpha    = alpha0
    var moondist = moondist0
    var istar    = .0
    var Xzm      = .0
    var Xo       = .0
    var Z        = .0
    var Zmoon    = .0
    var Bmoon    = .0
    var fofrho   = .0
    var rho_rad  = .0 //,test;
    rho_rad = rho / DegsInRadian
    alpha = 180.0 - alpha
    Zmoon = (90.0 - altmoon) / DegsInRadian
    Z = (90.0 - alt) / DegsInRadian
    moondist = moondist / 60.27 /* divide by mean distance */
    istar = -0.4 * (3.84 + 0.026 * Math
      .abs(alpha) + 4.0e-9 * Math.pow(alpha, 4.0)) /*eqn 20*/
    istar = Math.pow(10.0, istar) / (moondist * moondist)
    if (Math.abs(alpha) < 7.0)
      /* crude accounting for opposition effect */ istar =
        istar * (1.35 - 0.05 * Math.abs(istar))
    /* 35 per cent brighter at full, effect tapering linearly to
               zero at 7 degrees away from full. mentioned peripherally in
               Krisciunas and Scheafer, p. 1035. */
    fofrho = 229087.0 * (1.06 + Math.cos(rho_rad) * Math.cos(rho_rad))
    if (Math.abs(rho) > 10.0)
      fofrho = fofrho + Math.pow(10.0, 6.15 - rho / 40.0) /* eqn 21 */
    else if (Math.abs(rho) > 0.25)
      fofrho = fofrho + 6.2e7 / (rho * rho) /* eqn 19 */
    else fofrho = fofrho + 9.9e8 /*for 1/4 degree -- radius of moon! */
    Xzm = Math.sqrt(1.0 - 0.96 * Math.sin(Zmoon) * Math.sin(Zmoon))
    if (Xzm != 0.0) Xzm = 1.0 / Xzm
    else Xzm = 10000.0
    Xo = Math.sqrt(1.0 - 0.96 * Math.sin(Z) * Math.sin(Z))
    if (Xo != 0.0) Xo = 1.0 / Xo
    else Xo = 10000.0
    Bmoon = fofrho * istar * Math.pow(10.0, -0.4 * KZen * Xzm) * (1.0 - Math
      .pow(10.0, -0.4 * KZen * Xo)) /* nanoLamberts */
    if (Bmoon > 0.001)
      22.50 - 1.08574 * Math.log(Bmoon / 34.08) /* V mag per sq arcs-eqn 1 */
    else 99.0
  }

  protected def accumoon(
    jd0:      Double,
    geolat:   Double,
    lst:      Double,
    elevsea:  Double,
    geora:    DoubleRef,
    geodec:   DoubleRef,
    geodist:  DoubleRef,
    topora:   DoubleRef,
    topodec:  DoubleRef,
    topodist: DoubleRef
  ): Unit = {
    var jd     = jd0
    /*      double *eclatit,*eclongit, *pie,*ra,*dec,*dist; geocent quantities,
                       formerly handed out but not in this version */
    var pie    = .0
    var dist   = .0
    /* horiz parallax */
    var Lpr    = .0
    var M      = .0
    var Mpr    = .0
    var D      = .0
    var F      = .0
    var Om     = .0
    var T      = .0
    var Tsq    = .0
    var Tcb    = .0
    var e      = .0
    var lambda = .0
    var B      = .0
    var beta   = .0
    var om1    = .0
    var om2    = .0
    var sinx   = .0
    var x      = .0
    var y      = .0
    var z      = .0
    val x_geo  = new DoubleRef
    val y_geo  = new DoubleRef
    val z_geo  = new DoubleRef
    //            double x_geo, y_geo, z_geo;  /* geocentric position of *observer* */
    jd = jd + etcorr(
      jd
    ) / SecsInDay /* approximate correction to ephemeris time */
    T = (jd - 2415020.0) / 36525.0 /* this based around 1900 ... */
    Tsq = T * T
    Tcb = Tsq * T
    Lpr = 270.434164 + 481267.8831 * T - 0.001133 * Tsq + 0.0000019 * Tcb
    M = 358.475833 + 35999.0498 * T - 0.000150 * Tsq - 0.0000033 * Tcb
    Mpr = 296.104608 + 477198.8491 * T + 0.009192 * Tsq + 0.0000144 * Tcb
    D = 350.737486 + 445267.1142 * T - 0.001436 * Tsq + 0.0000019 * Tcb
    F = 11.250889 + 483202.0251 * T - 0.003211 * Tsq - 0.0000003 * Tcb
    Om = 259.183275 - 1934.1420 * T + 0.002078 * Tsq + 0.0000022 * Tcb
    Lpr = circulo(Lpr)
    Mpr = circulo(Mpr)
    M = circulo(M)
    D = circulo(D)
    F = circulo(F)
    Om = circulo(Om)
    sinx = Math.sin((51.2 + 20.2 * T) / DegsInRadian)
    Lpr = Lpr + 0.000233 * sinx
    M = M - 0.001778 * sinx
    Mpr = Mpr + 0.000817 * sinx
    D = D + 0.002011 * sinx
    sinx =
      0.003964 * Math.sin(
        (346.560 + 132.870 * T - 0.0091731 * Tsq) / DegsInRadian
      )
    Lpr = Lpr + sinx
    Mpr = Mpr + sinx
    D = D + sinx
    F = F + sinx
    sinx = Math.sin(Om / DegsInRadian)
    Lpr = Lpr + 0.001964 * sinx
    Mpr = Mpr + 0.002541 * sinx
    D = D + 0.001964 * sinx
    F = F - 0.024691 * sinx
    F = F - 0.004328 * Math.sin((Om + 275.05 - 2.30 * T) / DegsInRadian)
    e = 1 - 0.002495 * T - 0.00000752 * Tsq
    M = M / DegsInRadian /* these will all be arguments ... */
    Mpr = Mpr / DegsInRadian
    D = D / DegsInRadian
    F = F / DegsInRadian
    lambda =
      Lpr + 6.288750 * Math.sin(Mpr) + 1.274018 * Math.sin(
        2 * D - Mpr
      ) + 0.658309 * Math.sin(
        2 * D
      ) + 0.213616 * Math.sin(2 * Mpr) - e * 0.185596 * Math.sin(
        M
      ) - 0.114336 * Math.sin(
        2 * F
      ) + 0.058793 * Math.sin(2 * D - 2 * Mpr) + e * 0.057212 * Math.sin(
        2 * D - M - Mpr
      ) + 0.053320 * Math.sin(2 * D + Mpr) + e * 0.045874 * Math.sin(
        2 * D - M
      ) + e * 0.041024 * Math.sin(Mpr - M) - 0.034718 * Math.sin(
        D
      ) - e * 0.030465 * Math.sin(
        M + Mpr
      ) + 0.015326 * Math.sin(2 * D - 2 * F) - 0.012528 * Math.sin(
        2 * F + Mpr
      ) - 0.010980 * Math.sin(2 * F - Mpr) + 0.010674 * Math.sin(
        4 * D - Mpr
      ) + 0.010034 * Math.sin(3 * Mpr) + 0.008548 * Math.sin(
        4 * D - 2 * Mpr
      ) - e * 0.007910 * Math.sin(M - Mpr + 2 * D) - e * 0.006783 * Math.sin(
        2 * D + M
      ) + 0.005162 * Math.sin(Mpr - D)
    /* And furthermore.....*/
    lambda = lambda + e * 0.005000 * Math.sin(M + D) + e * 0.004049 * Math.sin(
      Mpr - M + 2 * D
    ) + 0.003996 * Math.sin(2 * Mpr + 2 * D) + 0.003862 * Math.sin(
      4 * D
    ) + 0.003665 * Math.sin(2 * D - 3 * Mpr) + e * 0.002695 * Math.sin(
      2 * Mpr - M
    ) + 0.002602 * Math.sin(Mpr - 2 * F - 2 * D) + e * 0.002396 * Math.sin(
      2 * D - M - 2 * Mpr
    ) - 0.002349 * Math.sin(Mpr + D) + e * e * 0.002249 * Math.sin(
      2 * D - 2 * M
    ) - e * 0.002125 * Math.sin(2 * Mpr + M) - e * e * 0.002079 * Math.sin(
      2 * M
    ) + e * e * 0.002059 * Math.sin(2 * D - Mpr - 2 * M) - 0.001773 * Math.sin(
      Mpr + 2 * D - 2 * F
    ) - 0.001595 * Math.sin(2 * F + 2 * D) + e * 0.001220 * Math.sin(
      4 * D - M - Mpr
    ) - 0.001110 * Math.sin(2 * Mpr + 2 * F) + 0.000892 * Math.sin(
      Mpr - 3 * D
    ) - e * 0.000811 * Math.sin(M + Mpr + 2 * D) + e * 0.000761 * Math.sin(
      4 * D - M - 2 * Mpr
    ) + e * e * 0.000717 * Math.sin(Mpr - 2 * M) + e * e * 0.000704 * Math.sin(
      Mpr - 2 * M - 2 * D
    ) + e * 0.000693 * Math.sin(M - 2 * Mpr + 2 * D) + e * 0.000598 * Math.sin(
      2 * D - M - 2 * F
    ) + 0.000550 * Math.sin(Mpr + 4 * D) + 0.000538 * Math.sin(
      4 * Mpr
    ) + e * 0.000521 * Math.sin(4 * D - M) + 0.000486 * Math.sin(2 * Mpr - D)
    /*              *eclongit = lambda;  */
    B = 5.128189 * Math.sin(F) + 0.280606 * Math.sin(Mpr + F) + 0.277693 * Math
      .sin(
        Mpr - F
      ) + 0.173238 * Math.sin(2 * D - F) + 0.055413 * Math.sin(
      2 * D + F - Mpr
    ) + 0.046272 * Math.sin(2 * D - F - Mpr) + 0.032573 * Math.sin(
      2 * D + F
    ) + 0.017198 * Math.sin(2 * Mpr + F) + 0.009267 * Math.sin(
      2 * D + Mpr - F
    ) + 0.008823 * Math.sin(2 * Mpr - F) + e * 0.008247 * Math.sin(
      2 * D - M - F
    ) + 0.004323 * Math.sin(2 * D - F - 2 * Mpr) + 0.004200 * Math.sin(
      2 * D + F + Mpr
    ) + e * 0.003372 * Math.sin(F - M - 2 * D) + 0.002472 * Math.sin(
      2 * D + F - M - Mpr
    ) + e * 0.002222 * Math.sin(2 * D + F - M) + e * 0.002072 * Math.sin(
      2 * D - F - M - Mpr
    ) + e * 0.001877 * Math.sin(F - M + Mpr) + 0.001828 * Math.sin(
      4 * D - F - Mpr
    ) - e * 0.001803 * Math.sin(F + M) - 0.001750 * Math.sin(
      3 * F
    ) + e * 0.001570 * Math.sin(
      Mpr - M - F
    ) - 0.001487 * Math.sin(F + D) - e * 0.001481 * Math.sin(
      F + M + Mpr
    ) + e * 0.001417 * Math.sin(F - M - Mpr) + e * 0.001350 * Math.sin(
      F - M
    ) + 0.001330 * Math.sin(F - D) + 0.001106 * Math.sin(
      F + 3 * Mpr
    ) + 0.001020 * Math.sin(
      4 * D - F
    ) + 0.000833 * Math.sin(F + 4 * D - Mpr)
    /* not only that, but */
    B = B + 0.000781 * Math.sin(Mpr - 3 * F) + 0.000670 * Math.sin(
      F + 4 * D - 2 * Mpr
    ) + 0.000606 * Math.sin(2 * D - 3 * F) + 0.000597 * Math.sin(
      2 * D + 2 * Mpr - F
    ) + e * 0.000492 * Math.sin(2 * D + Mpr - M - F) + 0.000450 * Math.sin(
      2 * Mpr - F - 2 * D
    ) + 0.000439 * Math.sin(3 * Mpr - F) + 0.000423 * Math.sin(
      F + 2 * D + 2 * Mpr
    ) + 0.000422 * Math.sin(2 * D - F - 3 * Mpr) - e * 0.000367 * Math.sin(
      M + F + 2 * D - Mpr
    ) - e * 0.000353 * Math.sin(M + F + 2 * D) + 0.000331 * Math.sin(
      F + 4 * D
    ) + e * 0.000317 * Math.sin(2 * D + F - M + Mpr) + e * e * 0.000306 * Math
      .sin(
        2 * D - 2 * M - F
      ) - 0.000283 * Math.sin(Mpr + 3 * F)
    om1 = 0.0004664 * Math.cos(Om / DegsInRadian)
    om2 = 0.0000754 * Math.cos((Om + 275.05 - 2.30 * T) / DegsInRadian)
    beta = B * (1.0 - om1 - om2)
    /*      *eclatit = beta; */
    pie = 0.950724 + 0.051818 * Math.cos(Mpr) + 0.009531 * Math.cos(
      2 * D - Mpr
    ) + 0.007843 * Math.cos(2 * D) + 0.002824 * Math.cos(
      2 * Mpr
    ) + 0.000857 * Math.cos(
      2 * D + Mpr
    ) + e * 0.000533 * Math.cos(2 * D - M) + e * 0.000401 * Math.cos(
      2 * D - M - Mpr
    ) + e * 0.000320 * Math.cos(Mpr - M) - 0.000271 * Math.cos(
      D
    ) - e * 0.000264 * Math.cos(
      M + Mpr
    ) - 0.000198 * Math.cos(2 * F - Mpr) + 0.000173 * Math.cos(
      3 * Mpr
    ) + 0.000167 * Math.cos(
      4 * D - Mpr
    ) - e * 0.000111 * Math.cos(M) + 0.000103 * Math.cos(
      4 * D - 2 * Mpr
    ) - 0.000084 * Math.cos(2 * Mpr - 2 * D) - e * 0.000083 * Math.cos(
      2 * D + M
    ) + 0.000079 * Math.cos(2 * D + 2 * Mpr) + 0.000072 * Math.cos(
      4 * D
    ) + e * 0.000064 * Math.cos(2 * D - M + Mpr) - e * 0.000063 * Math.cos(
      2 * D + M - Mpr
    ) + e * 0.000041 * Math.cos(M + D) + e * 0.000035 * Math.cos(
      2 * Mpr - M
    ) - 0.000033 * Math.cos(3 * Mpr - 2 * D) - 0.000030 * Math.cos(
      Mpr + D
    ) - 0.000029 * Math.cos(2 * F - 2 * D) - e * 0.000029 * Math.cos(
      2 * Mpr + M
    ) + e * e * 0.000026 * Math.cos(2 * D - 2 * M) - 0.000023 * Math.cos(
      2 * F - 2 * D + Mpr
    ) + e * 0.000019 * Math.cos(4 * D - M - Mpr)
    beta = beta / DegsInRadian
    lambda = lambda / DegsInRadian
    val l      = new DoubleRef(Math.cos(lambda) * Math.cos(beta))
    val m      = new DoubleRef(Math.sin(lambda) * Math.cos(beta))
    val n      = new DoubleRef(Math.sin(beta))
    eclrot(jd, l, m, n)
    dist = 1 / Math.sin(pie / DegsInRadian)
    x = l.d * dist
    y = m.d * dist
    z = n.d * dist
    geora.d = atan_circ(l.d, m.d) * HrsInRadian
    geodec.d = Math.asin(n.d) * DegsInRadian
    geodist.d = dist
    geocent(lst, geolat, elevsea, x_geo, y_geo, z_geo)
    x = x - x_geo.d /* topocentric correction using elliptical earth fig. */
    y = y - y_geo.d
    z = z - z_geo.d
    topodist.d = Math.sqrt(x * x + y * y + z * z)
    l.d = x / topodist.d
    m.d = y / topodist.d
    n.d = z / topodist.d
    topora.d = atan_circ(l.d, m.d) * HrsInRadian
    topodec.d = Math.asin(n.d) * DegsInRadian
  }

  protected def ztwilight(alt: Double): Double = {
    /*
     * evaluates a polynomial expansion for the approximate brightening in
     * magnitudes of the zenith in twilight compared to its value at full
     * night, as function of altitude of the sun (in degrees). To get this
     * expression I looked in Meinel, A., & Meinel, M., "Sunsets, Twilight, &
     * Evening Skies", Cambridge U. Press, 1983; there's a graph on p. 38
     * showing the decline of zenith twilight. I read points off this graph
     * and fit them with a polynomial; I don't even know what band there
     * data are for!
     */ /*
     * Comparison with Ashburn, E. V. 1952, JGR, v.57, p.85 shows that this
     * is a good fit to his B-band measurements.
     */
    var y     = .0
    var `val` = .0
    y = (-1.0 * alt - 9.0) / 9.0 /* my polynomial's argument... */
    `val` = ((2.0635175 * y + 1.246602) * y - 9.4084495) * y + 6.132725
    `val`
  }

  protected def subtend(
    ra01:  Double,
    dec01: Double,
    ra02:  Double,
    dec02: Double
  ): Double = {
    var ra1   = ra01
    var dec1  = dec01
    var ra2   = ra02
    var dec2  = dec02
    /*
     * angle subtended by two positions in the sky -- return value is in
     * radians. Hybrid algorithm works down to zero separation except very
     * near the poles.
     */
    var x1    = .0
    var y1    = .0
    var z1    = .0
    var x2    = .0
    var y2    = .0
    var z2    = .0
    var theta = .0
    ra1 = ra1 / HrsInRadian
    dec1 = dec1 / DegsInRadian
    ra2 = ra2 / HrsInRadian
    dec2 = dec2 / DegsInRadian
    x1 = Math.cos(ra1) * Math.cos(dec1)
    y1 = Math.sin(ra1) * Math.cos(dec1)
    z1 = Math.sin(dec1)
    x2 = Math.cos(ra2) * Math.cos(dec2)
    y2 = Math.sin(ra2) * Math.cos(dec2)
    z2 = Math.sin(dec2)
    theta = Math.acos(x1 * x2 + y1 * y2 + z1 * z2)
    /*
     * use flat Pythagorean approximation if the angle is very small and*
     * you're not close to the pole; avoids roundoff in arccos.
     */
    if (theta < 1.0e-5)
      /* seldom the case, so don't combine test */
      if (
        Math.abs(dec1) < (Math.PI / 2.0 - 0.001) && Math.abs(dec2) < (
          Math.PI / 2.0 - 0.001
        )
      ) {
        /* recycled variables here... */
        x1 = (ra2 - ra1) * Math.cos((dec1 + dec2) / 2.0)
        x2 = dec2 - dec1
        theta = Math.sqrt(x1 * x1 + x2 * x2)
      }
    theta
  }
}
