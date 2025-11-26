// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

trait VoTableSamples {

  lazy val fieldsNode =
    <TABLE>
        <FIELD ID="gmag_err" datatype="double" name="gmag_err" ucd="stat.error;phot.mag;em.opt.g"/>
        <FIELD ID="rmag_err" datatype="double" name="rmag_err" ucd="stat.error;phot.mag;em.opt.r"/>
        <FIELD ID="flags1" datatype="int" name="flags1" ucd="meta.code"/>
        <FIELD ID="ppmxl" datatype="int" name="ppmxl" ucd="meta.id;meta.main"/>
      </TABLE>

  lazy val tableRow =
    <TR>
        <TD>0.0960165</TD>
        <TD>0.0503736</TD>
        <TD>268435728</TD>
        <TD>-2140405448</TD>
      </TR>

  lazy val tableRowMissing =
    <TR>
        <TD>0.0960165</TD>
        <TD>0.0503736</TD>
        <TD>268435728</TD>
      </TR>

  lazy val tableRowExtra =
    <TR>
        <TD>0.0960165</TD>
        <TD>0.0503736</TD>
        <TD>268435728</TD>
        <TD>-2140405448</TD>
        <TD>-2140405448</TD>
      </TR>

  lazy val dataNode =
    <DATA>
        <TABLEDATA>
          <TR>
            <TD>0.0960165</TD>
            <TD>0.0503736</TD>
            <TD>268435728</TD>
            <TD>-2140405448</TD>
          </TR>
          <TR>
            <TD>0.51784</TD>
            <TD>0.252201</TD>
            <TD>536871168</TD>
            <TD>-2140404569</TD>
          </TR>
        </TABLEDATA>
      </DATA>

  lazy val dataNodeMissing = <DATA>
        <TABLEDATA>
          <TR>
            <TD>0.0960165</TD>
            <TD>0.0503736</TD>
            <TD>268435728</TD>
          </TR>
          <TR>
            <TD>0.51784</TD>
            <TD>0.252201</TD>
            <TD>536871168</TD>
            <TD>-2140404569</TD>
          </TR>
        </TABLEDATA>
      </DATA>

  lazy val targets =
    <TABLE>
        <FIELD ID="flags1" datatype="int" name="flags1" ucd="meta.code"/>
        <FIELD ID="umag" datatype="double" name="umag" ucd="phot.mag;em.opt.u"/>
        <FIELD ID="flags2" datatype="int" name="flags2" ucd="meta.code"/>
        <FIELD ID="imag" datatype="double" name="imag" ucd="phot.mag;em.opt.i"/>
        <FIELD ID="decj2000" datatype="double" name="dej2000" ucd="pos.eq.dec;meta.main"/>
        <FIELD ID="raj2000" datatype="double" name="raj2000" ucd="pos.eq.ra;meta.main"/>
        <FIELD ID="rmag" datatype="double" name="rmag" ucd="phot.mag;em.opt.r"/>
        <FIELD ID="objid" datatype="int" name="objid" ucd="meta.id;meta.main"/>
        <FIELD ID="gmag" datatype="double" name="gmag" ucd="phot.mag;em.opt.g"/>
        <FIELD ID="zmag" datatype="double" name="zmag" ucd="phot.mag;em.opt.z"/>
        <FIELD ID="type" datatype="int" name="type" ucd="meta.code"/>
        <FIELD ID="ppmxl" datatype="int" name="ppmxl" ucd="meta.id;meta.main"/>
        <DATA>
          <TABLEDATA>
            <TR>
              <TD>268435728</TD>
              <TD>23.0888</TD>
              <TD>8208</TD>
              <TD>20.3051</TD>
              <TD>0.209323681906</TD>
              <TD>359.745951955</TD>
              <TD>20.88</TD>
              <TD>-2140405448</TD>
              <TD>22.082</TD>
              <TD>19.8812</TD>
              <TD>3</TD>
              <TD>-2140405448</TD>
            </TR>
            <TR>
              <TD>536871168</TD>
              <TD>23.0853</TD>
              <TD>65552</TD>
              <TD>20.7891</TD>
              <TD>0.210251239819</TD>
              <TD>359.749274134</TD>
              <TD>21.7686</TD>
              <TD>-2140404569</TD>
              <TD>23.0889</TD>
              <TD>20.0088</TD>
              <TD>3</TD>
              <TD>-2140404569</TD>
            </TR>
          </TABLEDATA>
      </DATA>
    </TABLE>

  lazy val targetsWithRedshift =
    <TABLE>
        <FIELD ID="flags1" datatype="int" name="flags1" ucd="meta.code"/>
        <FIELD ID="umag" datatype="double" name="umag" ucd="phot.mag;em.opt.u"/>
        <FIELD ID="flags2" datatype="int" name="flags2" ucd="meta.code"/>
        <FIELD ID="imag" datatype="double" name="imag" ucd="phot.mag;em.opt.i"/>
        <FIELD ID="decj2000" datatype="double" name="dej2000" ucd="pos.eq.dec;meta.main"/>
        <FIELD ID="raj2000" datatype="double" name="raj2000" ucd="pos.eq.ra;meta.main"/>
        <FIELD ID="rmag" datatype="double" name="rmag" ucd="phot.mag;em.opt.r"/>
        <FIELD ID="objid" datatype="int" name="objid" ucd="meta.id;meta.main"/>
        <FIELD ID="gmag" datatype="double" name="gmag" ucd="phot.mag;em.opt.g"/>
        <FIELD ID="zmag" datatype="double" name="zmag" ucd="phot.mag;em.opt.z"/>
        <FIELD ID="type" datatype="int" name="type" ucd="meta.code"/>
        <FIELD ID="ppmxl" datatype="int" name="ppmxl" ucd="meta.id;meta.main"/>
        <FIELD ID="Z_VALUE" datatype="double" name="Z_VALUE" ucd="src.redshift"/>
        <FIELD ID="RV_VALUE" datatype="double" name="RV_VALUE" ucd="spect.dopplerVeloc.opt"/>
        <DATA>
          <TABLEDATA>
            <TR>
              <TD>268435728</TD>
              <TD>23.0888</TD>
              <TD>8208</TD>
              <TD>20.3051</TD>
              <TD>0.209323681906</TD>
              <TD>359.745951955</TD>
              <TD>20.88</TD>
              <TD>-2140405448</TD>
              <TD>22.082</TD>
              <TD>19.8812</TD>
              <TD>3</TD>
              <TD>-2140405448</TD>
              <TD>0.000068</TD>
              <TD></TD>
            </TR>
            <TR>
              <TD>536871168</TD>
              <TD>23.0853</TD>
              <TD>65552</TD>
              <TD>20.7891</TD>
              <TD>0.210251239819</TD>
              <TD>359.749274134</TD>
              <TD>21.7686</TD>
              <TD>-2140404569</TD>
              <TD>23.0889</TD>
              <TD>20.0088</TD>
              <TD>3</TD>
              <TD>-2140404569</TD>
              <TD></TD>
              <TD>20.30</TD>
            </TR>
          </TABLEDATA>
      </DATA>
    </TABLE>

  lazy val targetsWithRadialVelocityError =
    <TABLE>
        <FIELD ID="flags1" datatype="int" name="flags1" ucd="meta.code"/>
        <FIELD ID="umag" datatype="double" name="umag" ucd="phot.mag;em.opt.u"/>
        <FIELD ID="flags2" datatype="int" name="flags2" ucd="meta.code"/>
        <FIELD ID="imag" datatype="double" name="imag" ucd="phot.mag;em.opt.i"/>
        <FIELD ID="decj2000" datatype="double" name="dej2000" ucd="pos.eq.dec;meta.main"/>
        <FIELD ID="raj2000" datatype="double" name="raj2000" ucd="pos.eq.ra;meta.main"/>
        <FIELD ID="rmag" datatype="double" name="rmag" ucd="phot.mag;em.opt.r"/>
        <FIELD ID="objid" datatype="int" name="objid" ucd="meta.id;meta.main"/>
        <FIELD ID="gmag" datatype="double" name="gmag" ucd="phot.mag;em.opt.g"/>
        <FIELD ID="zmag" datatype="double" name="zmag" ucd="phot.mag;em.opt.z"/>
        <FIELD ID="type" datatype="int" name="type" ucd="meta.code"/>
        <FIELD ID="ppmxl" datatype="int" name="ppmxl" ucd="meta.id;meta.main"/>
        <FIELD ID="RV_VALUE" datatype="double" name="RV_VALUE" ucd="spect.dopplerVeloc.opt"/>
        <DATA>
          <TABLEDATA>
            <TR>
              <TD>536871168</TD>
              <TD>23.0853</TD>
              <TD>65552</TD>
              <TD>20.7891</TD>
              <TD>0.210251239819</TD>
              <TD>359.749274134</TD>
              <TD>21.7686</TD>
              <TD>-2140404569</TD>
              <TD>23.0889</TD>
              <TD>20.0088</TD>
              <TD>3</TD>
              <TD>-2140404569</TD>
              <TD>299792.459</TD>
            </TR>
          </TABLEDATA>
      </DATA>
    </TABLE>

  lazy val targetsWithErrors =
    <TABLE>
        <FIELD ID="gmag_err" datatype="double" name="gmag_err" ucd="stat.error;phot.mag;em.opt.g"/>
        <FIELD ID="rmag_err" datatype="double" name="rmag_err" ucd="stat.error;phot.mag;em.opt.r"/>
        <FIELD ID="flags1" datatype="int" name="flags1" ucd="meta.code"/>
        <FIELD ID="umag" datatype="double" name="umag" ucd="phot.mag;em.opt.u"/>
        <FIELD ID="flags2" datatype="int" name="flags2" ucd="meta.code"/>
        <FIELD ID="imag" datatype="double" name="imag" ucd="phot.mag;em.opt.i"/>
        <FIELD ID="zmag_err" datatype="double" name="zmag_err" ucd="stat.error;phot.mag;em.opt.z"/>
        <FIELD ID="decj2000" datatype="double" name="dej2000" ucd="pos.eq.dec;meta.main"/>
        <FIELD ID="umag_err" datatype="double" name="umag_err" ucd="stat.error;phot.mag;em.opt.u"/>
        <FIELD ID="imag_err" datatype="double" name="imag_err" ucd="stat.error;phot.mag;em.opt.i"/>
        <FIELD ID="raj2000" datatype="double" name="raj2000" ucd="pos.eq.ra;meta.main"/>
        <FIELD ID="rmag" datatype="double" name="rmag" ucd="phot.mag;em.opt.r"/>
        <FIELD ID="objid" datatype="int" name="objid" ucd="meta.id;meta.main"/>
        <FIELD ID="gmag" datatype="double" name="gmag" ucd="phot.mag;em.opt.g"/>
        <FIELD ID="zmag" datatype="double" name="zmag" ucd="phot.mag;em.opt.z"/>
        <FIELD ID="type" datatype="int" name="type" ucd="meta.code"/>
        <FIELD ID="jmag" datatype="double" name="jmag" ucd="phot.mag;em.IR.J"/>
        <FIELD ID="e_jmag" datatype="double" name="e_jmag" ucd="stat.error;phot.mag;em.IR.J"/>
        <FIELD ID="ppmxl" datatype="int" name="ppmxl" ucd="meta.id;meta.main"/>
        <DATA>
          <TABLEDATA>
            <TR>
              <TD>0.0960165</TD>
              <TD>0.0503736</TD>
              <TD>268435728</TD>
              <TD>23.0888</TD>
              <TD>8208</TD>
              <TD>20.3051</TD>
              <TD>0.138202</TD>
              <TD>0.209323681906</TD>
              <TD>0.518214</TD>
              <TD>0.0456069</TD>
              <TD>359.745951955</TD>
              <TD>20.88</TD>
              <TD>-2140405448</TD>
              <TD>22.082</TD>
              <TD>19.8812</TD>
              <TD>3</TD>
              <TD>13.74</TD>
              <TD>0.029999999999999999</TD>
              <TD>-2140405448</TD>
            </TR>
            <TR>
              <TD>0.51784</TD>
              <TD>0.252201</TD>
              <TD>536871168</TD>
              <TD>23.0853</TD>
              <TD>65552</TD>
              <TD>20.7891</TD>
              <TD>0.35873</TD>
              <TD>0.210251239819</TD>
              <TD>1.20311</TD>
              <TD>0.161275</TD>
              <TD>359.749274134</TD>
              <TD>21.7686</TD>
              <TD>-2140404569</TD>
              <TD>23.0889</TD>
              <TD>20.0088</TD>
              <TD>3</TD>
              <TD>12.023</TD>
              <TD>0.02</TD>
              <TD>-2140404569</TD>
            </TR>
          </TABLEDATA>
      </DATA>
    </TABLE>

  lazy val targetsWithProperMotion =
    <TABLE>
        <FIELD ID="pmde" datatype="double" name="pmde" ucd="pos.pm;pos.eq.dec"/>
        <FIELD ID="pmra" datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra"/>
        <FIELD ID="dej2000" datatype="double" name="dej2000" ucd="pos.eq.dec;meta.main"/>
        <FIELD ID="epde" datatype="double" name="epde" ucd="time.epoch"/>
        <FIELD ID="raj2000" datatype="double" name="raj2000" ucd="pos.eq.ra;meta.main"/>
        <FIELD ID="rmag" datatype="double" name="rmag" ucd="phot.mag;em.opt.R"/>
        <FIELD ID="e_vmag" datatype="double" name="e_vmag" ucd="stat.error;phot.mag;em.opt.V"/>
        <FIELD ID="e_pmra" datatype="double" name="e_pmra" ucd="stat.error;pos.pm;pos.eq.ra"/>
        <FIELD ID="ucac4" arraysize="*" datatype="char" name="ucac4" ucd="meta.id;meta.main"/>
        <FIELD ID="epra" datatype="double" name="epra" ucd="time.epoch"/>
        <FIELD ID="e_pmde" datatype="double" name="e_pmde" ucd="stat.error;pos.pm;pos.eq.dec"/>
        <DATA>
         <TABLEDATA>
          <TR>
           <TD>-4.9000000000000004</TD>
           <TD>-10.199999999999999</TD>
           <TD>19.9887894444444</TD>
           <TD>2000.3499999999999</TD>
           <TD>9.8971419444444404</TD>
           <TD>14.76</TD>
           <TD>0.02</TD>
           <TD>3.8999999999999999</TD>
           <TD>550-001323</TD>
           <TD>1999.9100000000001</TD>
            <TD>2.4345</TD>
          </TR>
          <TR>
           <TD>-13.9</TD>
           <TD>-7</TD>
           <TD>19.997709722222201</TD>
           <TD>2000.0699999999999</TD>
           <TD>9.9195805555555605</TD>
           <TD>12.983000000000001</TD>
           <TD>0.029999999999999999</TD>
           <TD>1.8</TD>
           <TD>550-001324</TD>
           <TD>1999.4300000000001</TD>
           <TD>2.3999999999999999</TD>
          </TR>
         </TABLEDATA>
        </DATA>
       </TABLE>

  lazy val gaia =
    <TABLE>
        <FIELD ID="DESIGNATION" arraysize="*" datatype="char" name="designation" ucd="meta.id;meta.main">
          <DESCRIPTION>Unique source designation (unique across all Data Releases)</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="ra" ref="GAIADR2" ucd="pos.eq.ra;meta.main" unit="deg" utype="Char.SpatialAxis.Coverage.Location.Coord.Position2D.Value2.C1">
          <DESCRIPTION>Right ascension</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="ra_error" ucd="stat.error;pos.eq.ra" unit="mas">
          <DESCRIPTION>Standard error of right ascension</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="dec" ref="GAIADR2" ucd="pos.eq.dec;meta.main" unit="deg" utype="Char.SpatialAxis.Coverage.Location.Coord.Position2D.Value2.C2">
          <DESCRIPTION>Declination</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="dec_error" ucd="stat.error;pos.eq.dec" unit="mas">
          <DESCRIPTION>Standard error of declination</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="parallax" ucd="pos.parallax.trig" unit="mas">
          <DESCRIPTION>Parallax</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas.yr**-1">
          <DESCRIPTION>Proper motion in right ascension direction</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="pmra_error" ucd="stat.error;pos.pm;pos.eq.ra" unit="mas.yr**-1">
          <DESCRIPTION>Standard error of proper motion in right ascension direction</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas.yr**-1">
          <DESCRIPTION>Proper motion in declination direction</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="pmdec_error" ucd="stat.error;pos.pm;pos.eq.dec" unit="mas.yr**-1">
          <DESCRIPTION>Standard error of proper motion in declination direction</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="ref_epoch" ucd="meta.ref;time.epoch" unit="yr">
          <DESCRIPTION>Reference epoch</DESCRIPTION>
        </FIELD>
        <FIELD datatype="float" name="phot_g_mean_mag" ucd="phot.mag;stat.mean;em.opt" unit="mag">
          <DESCRIPTION>G-band mean magnitude</DESCRIPTION>
        </FIELD>
        <FIELD datatype="float" name="bp_rp" ucd="phot.color" unit="mag">
          <DESCRIPTION>BP - RP colour</DESCRIPTION>
        </FIELD>
        <FIELD datatype="double" name="radial_velocity" ucd="spect.dopplerVeloc.opt" unit="km.s**-1">
          <DESCRIPTION>Radial velocity</DESCRIPTION>
        </FIELD>
        <DATA>
          <TABLEDATA>
            <TR>
              <TD>Gaia DR2 5500810292414804352</TD>
              <TD>95.97543693997628</TD>
              <TD>0.8972436225190542</TD>
              <TD>-52.74602088557901</TD>
              <TD>1.1187287208599193</TD>
              <TD>-0.059333971256738484</TD>
              <TD>5.444032860309618</TD>
              <TD>2.0096218591421637</TD>
              <TD>2.412759805075276</TD>
              <TD>2.292112882376078</TD>
              <TD>2015.5</TD>
              <TD>19.782911</TD>
              <TD></TD> <!-- No BP - RP means no magnitude information -->
              <TD></TD>
            </TR>
            <TR>
              <TD>Gaia DR2 5500810842175280768</TD>
              <TD>96.07794677734371</TD>
              <TD>1.7974083970121115</TD>
              <TD>-52.752866472994484</TD>
              <TD>1.3361631129404261</TD>
              <TD></TD>
              <TD></TD>
              <TD></TD>
              <TD></TD>
              <TD></TD>
              <TD>2015.5</TD>
              <TD></TD> <!-- No G-band means no magnitude information -->
              <TD></TD>
              <TD></TD>
            </TR>
            <TR>
              <TD>Gaia DR2 5500810223699979264</TD>
              <TD>95.96329279548434</TD>
              <TD>0.01360005536042634</TD>
              <TD>-52.77304994651542</TD>
              <TD>0.01653042640473304</TD>
              <TD>1.0777658952216769</TD>
              <TD>-0.8181139364821904</TD>
              <TD>0.028741305378710533</TD>
              <TD>12.976157539714205</TD>
              <TD>0.031294621220519486</TD>
              <TD>2015.5</TD>
              <TD>13.91764</TD>
              <TD>2.68324375</TD>
              <TD></TD>
            </TR>
            <TR>
              <TD>Gaia DR2 5500810326779190016</TD>
              <TD>95.98749097569124</TD>
              <TD>0.0862887211183082</TD>
              <TD>-52.741666247338124</TD>
              <TD>0.09341802945058283</TD>
              <TD>3.6810721649521616</TD>
              <TD>6.456830239423608</TD>
              <TD>0.19897351485381112</TD>
              <TD>22.438383124975978</TD>
              <TD>0.18174463860202664</TD>
              <TD>2015.5</TD>
              <TD>14.292543</TD>
              <TD>1.0745363</TD>
              <TD>20.30</TD>  <!-- Radial velocity -->
            </TR>
          </TABLEDATA>
        </DATA>
      </TABLE>

  lazy val voTable =
    <VOTABLE>
        <RESOURCE type="results">
          {targets}
        </RESOURCE>
      </VOTABLE>

  lazy val voTableWithErrors =
    <VOTABLE>
        <RESOURCE type="results">
          {targetsWithErrors}
        </RESOURCE>
      </VOTABLE>

  lazy val voTableWithProperMotion =
    <VOTABLE>
        <RESOURCE type="results">
          {targetsWithProperMotion}
        </RESOURCE>
      </VOTABLE>

  lazy val voTableWithRedshift =
    <VOTABLE>
        <RESOURCE type="results">
          {targetsWithRedshift}
        </RESOURCE>
      </VOTABLE>

  lazy val voTableWithRadialVelocityError =
    <VOTABLE>
        <RESOURCE type="results">
          {targetsWithRadialVelocityError}
        </RESOURCE>
      </VOTABLE>

  lazy val voTableGaia =
    <VOTABLE>
        <RESOURCE type="results">
          {gaia}
        </RESOURCE>
      </VOTABLE>

  lazy val voTableGaiaPMCorrected =
    <VOTABLE version="1.4" xmlns="http://www.ivoa.net/xml/VOTable/v1.3" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.3 http://www.ivoa.net/xml/VOTable/v1.3">
      <RESOURCE type="results">
      <INFO name="QUERY_STATUS" value="OK" />

      <INFO name="QUERY" value="SELECT TOP 1 designation,pmra,pmdec,ref_epoch,parallax,radial_velocity,phot_g_mean_mag,phot_bp_mean_mag,phot_rp_mean_mag ,COORD1(EPOCH_PROP_POS(ra, dec, parallax, pmra, pmdec, radial_velocity, 2015.5, 2000)) as ra,COORD2(EPOCH_PROP_POS(ra, dec, parallax, pmra, pmdec, radial_velocity, 2015.5, 2000)) as dec
          FROM gaiadr2.gaia_source
          WHERE CONTAINS(POINT(&#039;ICRS&#039;,ra,dec),CIRCLE(&#039;ICRS&#039;, 244.26004167, -22.97608333, 0.04083333))=1

            "><![CDATA[SELECT TOP 1 designation,pmra,pmdec,ref_epoch,parallax,radial_velocity,phot_g_mean_mag,phot_bp_mean_mag,phot_rp_mean_mag ,COORD1(EPOCH_PROP_POS(ra, dec, parallax, pmra, pmdec, radial_velocity, 2015.5, 2000)) as ra,COORD2(EPOCH_PROP_POS(ra, dec, parallax, pmra, pmdec, radial_velocity, 2015.5, 2000)) as dec
          FROM gaiadr2.gaia_source
          WHERE CONTAINS(POINT('ICRS',ra,dec),CIRCLE('ICRS', 244.26004167, -22.97608333, 0.04083333))=1

            ]]></INFO>
      <INFO name="CAPTION" value="How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html"><![CDATA[How to cite and acknowledge Gaia: https://gea.esac.esa.int/archive/documentation/credits.html]]></INFO>
      <INFO name="PAGE" value="" />
      <INFO name="PAGE_SIZE" value="" />
      <INFO name="JOBID" value="1649874610452O"><![CDATA[1649874610452O]]></INFO>
      <INFO name="JOBNAME" value="" />

      <TABLE>
      <FIELD ID="DESIGNATION" arraysize="*" datatype="char" name="designation" ucd="meta.id;meta.main">
      <DESCRIPTION>Unique source designation (unique across all Data Releases)</DESCRIPTION>
      </FIELD>
      <FIELD datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas.yr**-1">
      <DESCRIPTION>Proper motion in right ascension direction</DESCRIPTION>
      </FIELD>
      <FIELD datatype="double" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas.yr**-1">
      <DESCRIPTION>Proper motion in declination direction</DESCRIPTION>
      </FIELD>
      <FIELD datatype="double" name="ref_epoch" ucd="meta.ref;time.epoch" unit="yr">
      <DESCRIPTION>Reference epoch</DESCRIPTION>
      </FIELD>
      <FIELD datatype="double" name="parallax" ucd="pos.parallax" unit="mas">
      <DESCRIPTION>Parallax</DESCRIPTION>
      </FIELD>
      <FIELD datatype="double" name="radial_velocity" ucd="spect.dopplerVeloc.opt" unit="km.s**-1">
      <DESCRIPTION>Radial velocity</DESCRIPTION>
      </FIELD>
      <FIELD datatype="float" name="phot_g_mean_mag" ucd="phot.mag;stat.mean;em.opt" unit="mag">
      <DESCRIPTION>G-band mean magnitude</DESCRIPTION>
      </FIELD>
      <FIELD datatype="float" name="phot_bp_mean_mag" ucd="phot.mag;stat.mean" unit="mag">
      <DESCRIPTION>Integrated BP mean magnitude</DESCRIPTION>
      </FIELD>
      <FIELD datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;stat.mean" unit="mag">
      <DESCRIPTION>Integrated RP mean magnitude</DESCRIPTION>
      </FIELD>
      <FIELD datatype="double" name="ra"/>
      <FIELD datatype="double" name="dec"/>
      <DATA>
      <TABLEDATA>
        <TR>
          <TD>Gaia DR2 6050423032358097664</TD>
          <TD></TD>
          <TD></TD>
          <TD>2015.5</TD>
          <TD></TD>
          <TD></TD>
          <TD>20.755217</TD>
          <TD></TD>
          <TD></TD>
          <TD>244.26317318202356</TD>
          <TD>-22.954945101383874</TD>
        </TR>
      </TABLEDATA>
      </DATA>

      </TABLE>
      </RESOURCE>
      <RESOURCE type="meta" utype="adhoc:service" name="ancillary">
          <DESCRIPTION>Retrieve DataLink file containing ancillary data for source</DESCRIPTION>
          <PARAM name="standardID" datatype="char" arraysize="*" value="ivo://ivoa.net/std/DataLink#links-1.0"/>
          <PARAM name="accessURL" datatype="char" arraysize="*" value="https://gea.esac.esa.int/data-server/datalink/links"/>
          <PARAM name="contentType" datatype="char" arraysize="*" value="application/x-votable+xml;content=datalink"/>
          <GROUP name="inputParams">
              <PARAM name="ID"  datatype="char" arraysize="*" value="" ref="DESIGNATION"/>
          </GROUP>
      </RESOURCE>
    </VOTABLE>

  // Sample for GAVO
  // parallax:              pos.parallax
  // radial_velocity_error: stat.error;spect.dopplerVeloc
  //
  // curl -s "https://dc.g-vo.org/__system__/tap/run/sync?REQUEST=doQuery&LANG=ADQL&FORMAT=votabletd&QUERY=SELECT%20source_id,ra,dec,pmra,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag%20FROM%20gaia.dr3lite%20WHERE%20source_id%20=%20538670232718296576"
  lazy val gavoParallaxAndRV =
    """<VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.3" version="1.5">
      <RESOURCE type="results">
        <TABLE name="dr3lite">
          <FIELD ID="source_id" datatype="long" name="source_id" ucd="meta.id;meta.main"/>
          <FIELD ID="ra" datatype="double" name="ra" ucd="pos.eq.ra;meta.main" unit="deg"/>
          <FIELD ID="dec" datatype="double" name="dec" ucd="pos.eq.dec;meta.main" unit="deg"/>
          <FIELD ID="pmra" datatype="float" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas/yr"/>
          <FIELD ID="pmdec" datatype="float" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas/yr"/>
          <FIELD ID="parallax" datatype="float" name="parallax" ucd="pos.parallax" unit="mas"/>
          <FIELD ID="radial_velocity" datatype="float" name="radial_velocity" ucd="spect.dopplerVeloc.opt;em.opt.I" unit="km/s"/>
          <FIELD ID="phot_g_mean_mag" datatype="float" name="phot_g_mean_mag" ucd="phot.mag;em.opt;stat.mean" unit="mag"/>
          <FIELD ID="phot_rp_mean_mag" datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;em.opt.R" unit="mag"/>
          <DATA>
            <TABLEDATA>
              <TR>
                <TD>538670232718296576</TD>
                <TD>11.041855402712585</TD>
                <TD>74.84504666323086</TD>
                <TD>-1.4343021</TD>
                <TD>-1.0645945</TD>
                <TD>0.16641381</TD>
                <TD>-39.225376</TD>
                <TD>15.083894</TD>
                <TD>14.250962</TD>
              </TR>
            </TABLEDATA>
          </DATA>
        </TABLE>
      </RESOURCE>
    </VOTABLE>"""

  // Alias for backwards compatibility
  lazy val gaiaWithRVAndParallax = gavoParallaxAndRV

  // Sample for ESA with gdr3_lite
  // parallax:        pos.parallax.trig
  // radial_velocity: spect.dopplerVeloc.opt;em.opt.I
  //
  // curl -s "https://gea.esac.esa.int/tap-server/tap/sync?REQUEST=doQuery&LANG=ADQL&FORMAT=votable_plain&QUERY=SELECT%20source_id,ra,pmra,dec,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag%20FROM%20gaiadr3.gaia_source_lite%20WHERE%20source_id%20=%20538670232718296576"
  lazy val esaLiteParallaxAndRV =
    """<VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.3" version="1.4">
      <RESOURCE type="results">
        <TABLE>
          <FIELD ID="SOURCE_ID" datatype="long" name="source_id" ucd="meta.id"/>
          <FIELD ID="ra" datatype="double" name="ra" ucd="pos.eq.ra;meta.main" unit="deg"/>
          <FIELD ID="pmra" datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas.yr**-1"/>
          <FIELD ID="dec" datatype="double" name="dec" ucd="pos.eq.dec;meta.main" unit="deg"/>
          <FIELD ID="pmdec" datatype="double" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas.yr**-1"/>
          <FIELD ID="parallax" datatype="double" name="parallax" ucd="pos.parallax.trig" unit="mas"/>
          <FIELD ID="radial_velocity" datatype="float" name="radial_velocity" ucd="spect.dopplerVeloc.opt;em.opt.I" unit="km.s**-1"/>
          <FIELD ID="phot_g_mean_mag" datatype="float" name="phot_g_mean_mag" ucd="phot.mag;em.opt" unit="mag"/>
          <FIELD ID="phot_rp_mean_mag" datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;em.opt.R" unit="mag"/>
          <DATA>
            <TABLEDATA>
              <TR>
                <TD>538670232718296576</TD>
                <TD>11.041855402712585</TD>
                <TD>-1.4343020524567958</TD>
                <TD>74.84504666323086</TD>
                <TD>-1.0645945622395614</TD>
                <TD>0.16641382003793037</TD>
                <TD>-39.225376</TD>
                <TD>15.083894</TD>
                <TD>14.250962</TD>
              </TR>
            </TABLEDATA>
          </DATA>
        </TABLE>
      </RESOURCE>
    </VOTABLE>"""

  // Gaia3Esa adapter votable sample
  // parallax:        pos.parallax.trig
  // radial_velocity: spect.dopplerVeloc.opt;em.opt.I
  //
  // curl -s "https://gea.esac.esa.int/tap-server/tap/sync?REQUEST=doQuery&LANG=ADQL&FORMAT=votable_plain&QUERY=SELECT%20designation,ra,pmra,dec,pmdec,ref_epoch,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag%20FROM%20gaiadr3.gaia_source%20WHERE%20source_id%20=%20538670232718296576"
  lazy val esaFullParallaxAndRV =
    """<VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.3" version="1.4">
      <RESOURCE type="results">
        <TABLE>
          <FIELD ID="DESIGNATION" arraysize="*" datatype="char" name="designation" ucd="meta.id;meta.main"/>
          <FIELD ID="ra" datatype="double" name="ra" ucd="pos.eq.ra;meta.main" unit="deg"/>
          <FIELD ID="pmra" datatype="double" name="pmra" ucd="pos.pm;pos.eq.ra" unit="mas.yr**-1"/>
          <FIELD ID="dec" datatype="double" name="dec" ucd="pos.eq.dec;meta.main" unit="deg"/>
          <FIELD ID="pmdec" datatype="double" name="pmdec" ucd="pos.pm;pos.eq.dec" unit="mas.yr**-1"/>
          <FIELD ID="ref_epoch" datatype="double" name="ref_epoch" ucd="meta.ref;time.epoch" unit="yr"/>
          <FIELD ID="parallax" datatype="double" name="parallax" ucd="pos.parallax.trig" unit="mas"/>
          <FIELD ID="radial_velocity" datatype="float" name="radial_velocity" ucd="spect.dopplerVeloc.opt;em.opt.I" unit="km.s**-1"/>
          <FIELD ID="phot_g_mean_mag" datatype="float" name="phot_g_mean_mag" ucd="phot.mag;em.opt" unit="mag"/>
          <FIELD ID="phot_rp_mean_mag" datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;em.opt.R" unit="mag"/>
          <DATA>
            <TABLEDATA>
              <TR>
                <TD>Gaia DR3 538670232718296576</TD>
                <TD>11.041855402712585</TD>
                <TD>-1.4343020524567958</TD>
                <TD>74.84504666323086</TD>
                <TD>-1.0645945622395614</TD>
                <TD>2016.0</TD>
                <TD>0.16641382003793037</TD>
                <TD>-39.225376</TD>
                <TD>15.083894</TD>
                <TD>14.250962</TD>
              </TR>
            </TABLEDATA>
          </DATA>
        </TABLE>
      </RESOURCE>
    </VOTABLE>"""

  lazy val voTableAlternative =
    <VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.3" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.5" xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.3 http://vo.ari.uni-heidelberg.de/docs/schemata/VOTable.xsd">
      <DESCRIPTION>
    This schema contains data re-published from the official
    Gaia mirrors (such as ivo://uni-heidelberg.de/gaia/tap) either to
    support combining its data with local tables (the various Xlite tables)
    or to make the data more accessible to VO clients (e.g., epoch fluxes).

    Other Gaia-related data is found in, among others, the gdr3mock,
    gdr3spec, gedr3auto, gedr3dist, gedr3mock, and gedr3spur schemas.</DESCRIPTION>
      <INFO name="legal" value=" If you use public Gaia DR3 data in a paper, please take note of `ESAC's guide`_ on how to acknowledge and cite it.  .. _ESAC's guide: https://gea.esac.esa.int/archive/documentation/GDR3/Miscellaneous/sec_credit_and_citation_instructions/"/>
      <RESOURCE type="results">
        <INFO name="sql_query" value="SELECT source_id, ra, pmra, dec, pmdec, parallax, radial_velocity, phot_g_mean_mag, phot_rp_mean_mag FROM gaia.dr3lite WHERE q3c_join(115.09507, - 17.41750, ra, dec, 0.08182) AND ( ( phot_rp_mean_mag &lt; 17.228 ) OR ( phot_g_mean_mag &lt; 17.228 ) ) AND ( ruwe &lt; 1.4 ) ORDER BY phot_g_mean_mag LIMIT 10">ADQL query translated to local SQL (for debugging)</INFO>
        <INFO name="query" value="SELECT TOP 10 source_id,ra,pmra,dec,pmdec,parallax,radial_velocity,phot_g_mean_mag,phot_rp_mean_mag       FROM gaia.dr3lite      WHERE CONTAINS(POINT('ICRS',ra,dec),CIRCLE('ICRS', 115.09507, -17.41750, 0.08182))=1      and ((phot_rp_mean_mag &lt; 17.228) or (phot_g_mean_mag &lt; 17.228))      and (ruwe &lt; 1.4)      ORDER BY phot_g_mean_mag       ">Original ADQL query</INFO>
        <INFO name="QUERY_STATUS" value="OK">Query successful</INFO>
        <INFO name="server_software" value="DaCHS/2.10 twistedWeb/22.4.0">Software that produced this VOTable</INFO>
        <INFO name="server" value="http://dc.g-vo.org">Base URI of the server</INFO>
        <INFO name="citation" ucd="" value="http://dc.g-vo.org/tableinfo/gaia.dr3lite#ti-citing">Advice on citing this resource</INFO>
        <INFO name="citation" ucd="" value="http://dc.g-vo.org/__system__/tap/run/howtocite">Advice on citing this resource</INFO>
        <INFO name="ivoid" ucd="meta.ref.ivoid" value="ivo://org.gavo.dc/gaia/q3/cone">Originating VO resource</INFO>
        <INFO name="ivoid" ucd="meta.ref.ivoid" value="ivo://org.gavo.dc/gaia/q3/dr3lite">Originating VO resource</INFO>
        <INFO name="ivoid" ucd="meta.ref.ivoid" value="ivo://org.gavo.dc/gaia/q3/edr3lite">Originating VO resource</INFO>
        <INFO name="ivoid_service" ucd="meta.ref.ivoid" value="ivo://org.gavo.dc/tap">Originating VO service</INFO>
        <INFO name="publisher" value="The GAVO DC team">Data centre that has delivered the data</INFO>
        <INFO name="rights" value=" If you use public Gaia DR3 data in a paper, please take note of `ESAC's guide`_ on how to acknowledge and cite it.  .. _ESAC's guide: https://gea.esac.esa.int/archive/documentation/GDR3/Miscellaneous/sec_credit_and_citation_instructions/">Legal conditions applicable to (parts of) the data contained</INFO>
        <INFO name="request_date" ucd="time.creation" value="2025-06-11T18:31:34Z"/>
        <INFO name="contact" ucd="meta.email" value="gavo@ari.uni-heidelberg.de">Contact option</INFO>
        <INFO name="reference_url" ucd="meta.ref.url" value="http://dc.g-vo.org/tableinfo/gaia.dr3lite">More information on the data Source</INFO>
        <INFO name="reference_url" ucd="meta.ref.url" value="http://dc.g-vo.org/__system__/tap/run/info">More information on the data Source</INFO>
        <INFO name="creator" ucd="meta.bib.author" value="GAIA Collaboration">Name of a person or entity that produced a contributing resource</INFO>
        <INFO name="creator" ucd="meta.bib.author" value="GAVO Data Centre">Name of a person or entity that produced a contributing resource</INFO>
        <COOSYS ID="system" epoch="J2016.0" refposition="BARYCENTER" system="ICRS"/>
        <TABLE name="dr3lite">
          <DESCRIPTION> This is gaia_source from the Gaia Data Release 3, stripped to just
    enough columns to enable basic science (but therefore a bit faster and
    simpler to deal with than the full gaia_source table).

    Note that on this server, there is also The gedr3dist.main, which
    gives distances computed by Bailer-Jones et al. Use these in
    preference to working with the raw parallaxes.

    This server also carries the gedr3mock schema containing a simulation
    of gaia_source based on a state-of-the-art galaxy model, computed by
    Rybizki et al.

    The full DR3 is available from numerous places in the VO (in
    particular from the TAP services ivo://uni-heidelberg.de/gaia/tap and
    ivo://esavo/gaia/tap).</DESCRIPTION>
          <GROUP utype="stc:CatalogEntryLocation">
            <PARAM arraysize="*" datatype="char" name="CoordFlavor" utype="stc:AstroCoordSystem.SpaceFrame.CoordFlavor" value="SPHERICAL"/>
            <PARAM arraysize="*" datatype="char" name="coord_naxes" utype="stc:AstroCoordSystem.SpaceFrame.CoordFlavor.coord_naxes" value="3"/>
            <PARAM arraysize="*" datatype="char" name="CoordRefFrame" utype="stc:AstroCoordSystem.SpaceFrame.CoordRefFrame" value="ICRS"/>
            <PARAM arraysize="*" datatype="char" name="ReferencePosition" utype="stc:AstroCoordSystem.SpaceFrame.ReferencePosition" value="BARYCENTER"/>
            <PARAM arraysize="*" datatype="char" name="Epoch" utype="stc:AstroCoords.Position3D.Epoch" value="2016.0"/>
            <PARAM arraysize="*" datatype="char" name="yearDef" utype="stc:AstroCoords.Position3D.Epoch.yearDef" value="J"/>
            <PARAM arraysize="*" datatype="char" name="URI" utype="stc:DataModel.URI" value="http://www.ivoa.net/xml/STC/stc-v1.30.xsd"/>
            <FIELDref ref="ra" utype="stc:AstroCoords.Position3D.Value3.C1"/>
            <FIELDref ref="dec" utype="stc:AstroCoords.Position3D.Value3.C2"/>
            <FIELDref ref="parallax" utype="stc:AstroCoords.Position3D.Value3.C3"/>
            <FIELDref ref="pmra" utype="stc:AstroCoords.Velocity3D.Value3.C1"/>
            <FIELDref ref="pmdec" utype="stc:AstroCoords.Velocity3D.Value3.C2"/>
            <FIELDref ref="radial_velocity" utype="stc:AstroCoords.Velocity3D.Value3.C3"/>
          </GROUP>
          <GROUP ID="ndtiluinwbea" name="note-id">
            <DESCRIPTION>
    For the contents of Gaia DR3,
    the source ID consists of a 64-bit integer, least
    significant bit = 1 and most significant bit = 64, comprising:

    * a HEALPix index number (sky pixel) in bits 36 - 63; by definition the
      smallest HEALPix index number is zero.
    * a 3-bit Data Processing Centre code in bits 33 - 35; for example
      MOD(source_id / 4294967296, 8) can be used to distinguish between
      sources initialised via the Initial Gaia Source List by the Torino DPC
      (code = 0) and sources otherwise detected and assigned by Gaia
      observations (code &gt; 0)
    * a 25-bit plus 7 bit sequence number within the HEALPix pixel in bits 1
      to 32 split into:

      * a 25 bit running number in bits 8 - 32; the running numbers are
        defined to be positive, i.e. never zero (except in the case of forced
        empty windows)
      * a 7-bit component number in bits 1 - 7

    This means that the HEALpix index level 12 of a given source is contained
    in the most significant bits. HEALpix index of 12 and lower levels can
    thus be retrieved as follows:

    * HEALpix level 12 = source_id / 34359738368
    * HEALpix level 11 = source_id / 137438953472
    * HEALpix level 10 = source_id / 549755813888
    * HEALpix level n = source_id / 2^35 * 4^(12 - level).</DESCRIPTION>
            <FIELDref ref="source_id"/>
          </GROUP>
          <FIELD ID="source_id" datatype="long" name="source_id" ucd="meta.id;meta.main">
            <DESCRIPTION>Gaia DR3 unique source identifier. Note that this *cannot* be matched against the DR1 or DR2 source_ids.</DESCRIPTION>
            <VALUES null="-1">
              <MIN value="1511828647680"/>
              <MAX value="6917528993283204480"/>
            </VALUES>
          </FIELD>
          <FIELD ID="ra" datatype="double" name="ra" ref="system" ucd="pos.eq.ra;meta.main" unit="deg">
            <DESCRIPTION>Barycentric Right Ascension in ICRS at epoch J2016.0</DESCRIPTION>
            <VALUES>
              <MIN value="1.6327128351173464e-06"/>
              <MAX value="359.9999839297149"/>
            </VALUES>
          </FIELD>
          <FIELD ID="pmra" datatype="float" name="pmra" ref="system" ucd="pos.pm;pos.eq.ra" unit="mas/yr">
            <DESCRIPTION>Proper motion in right ascension of the source in ICRS at J2016.0. This is the tangent plane projection (i.e., multiplied by cos(δ)) of the proper motion vector in the direction of increasing right ascension.</DESCRIPTION>
            <VALUES>
              <MIN value="-2290.752"/>
              <MAX value="4002.6545"/>
            </VALUES>
          </FIELD>
          <FIELD ID="dec" datatype="double" name="dec" ref="system" ucd="pos.eq.dec;meta.main" unit="deg">
            <DESCRIPTION>Barycentric Declination in ICRS at epoch J2016.0</DESCRIPTION>
            <VALUES>
              <MIN value="-89.855906626967"/>
              <MAX value="89.8088601480769"/>
            </VALUES>
          </FIELD>
          <FIELD ID="pmdec" datatype="float" name="pmdec" ref="system" ucd="pos.pm;pos.eq.dec" unit="mas/yr">
            <DESCRIPTION>Proper motion in declination at J2016.0.</DESCRIPTION>
            <VALUES>
              <MIN value="-5817.8003"/>
              <MAX value="1707.6958"/>
            </VALUES>
          </FIELD>
          <FIELD ID="parallax" datatype="float" name="parallax" ref="system" ucd="pos.parallax" unit="mas">
            <DESCRIPTION>Absolute barycentric stellar parallax of the source at the reference epoch J2016.0. If looking for a distance, consider joining with gedr3dist.main and using the distances from there.</DESCRIPTION>
            <VALUES>
              <MIN value="-187.0294"/>
              <MAX value="269.0573"/>
            </VALUES>
          </FIELD>
          <FIELD ID="radial_velocity" datatype="float" name="radial_velocity" ref="system" ucd="spect.dopplerVeloc.opt;em.opt.I" unit="km/s">
            <DESCRIPTION>Spectroscopic radial velocity in the solar barycentric reference frame. For stars brighter than about 12 mag, this is the median of all single-epoch measurements. For fainter stars, RV estimation is from a co-added spectrum.</DESCRIPTION>
            <VALUES>
              <MIN value="-861.1135"/>
              <MAX value="859.6903"/>
            </VALUES>
          </FIELD>
          <FIELD ID="phot_g_mean_mag" datatype="float" name="phot_g_mean_mag" ucd="phot.mag;em.opt;stat.mean" unit="mag">
            <DESCRIPTION>Mean magnitude in the G band. This is computed from the G-band mean flux applying the magnitude zero-point in the Vega scale. To obtain error estimates, see phot_g_mean_flux_over_error.</DESCRIPTION>
            <VALUES>
              <MIN value="1.7732803"/>
              <MAX value="22.75072"/>
            </VALUES>
          </FIELD>
          <FIELD ID="phot_rp_mean_mag" datatype="float" name="phot_rp_mean_mag" ucd="phot.mag;em.opt.R" unit="mag">
            <DESCRIPTION>Mean magnitude in the integrated RP band. This is computed from the RP-band mean flux applying the magnitude zero-point in the Vega scale. To obtain error estimates, see phot_rp_mean_flux_over_error.</DESCRIPTION>
            <VALUES>
              <MIN value="1.9687437"/>
              <MAX value="24.64073"/>
            </VALUES>
          </FIELD>
          <DATA>
            <TABLEDATA>
              <TR>
                <TD>5717302551387263616</TD>
                <TD>115.13078509503596</TD>
                <TD>-4.5898952</TD>
                <TD>-17.378470943968324</TD>
                <TD>-32.278133</TD>
                <TD>3.401994</TD>
                <TD>19.551666</TD>
                <TD>10.432884</TD>
                <TD>9.963753</TD>
              </TR>
              <TR>
                <TD>5717290422398546944</TD>
                <TD>115.15809787648688</TD>
                <TD>-0.8461862</TD>
                <TD>-17.41198932433166</TD>
                <TD>2.862218</TD>
                <TD>0.2685737</TD>
                <TD>90.652725</TD>
                <TD>11.320968</TD>
                <TD>9.906626</TD>
              </TR>
              <TR>
                <TD>5717278529623194752</TD>
                <TD>115.1056213385861</TD>
                <TD>-3.4439244</TD>
                <TD>-17.4536731554781</TD>
                <TD>1.8140981</TD>
                <TD>0.43036488</TD>
                <TD>76.312164</TD>
                <TD>12.15546</TD>
                <TD>11.282522</TD>
              </TR>
              <TR>
                <TD>5717277984171412608</TD>
                <TD>115.05828105654949</TD>
                <TD>-5.0840497</TD>
                <TD>-17.48108295111334</TD>
                <TD>5.6764627</TD>
                <TD>0.898946</TD>
                <TD>30.774696</TD>
                <TD>12.5115</TD>
                <TD>12.140568</TD>
              </TR>
              <TR>
                <TD>5717266885975823616</TD>
                <TD>115.11825700764322</TD>
                <TD>7.877352</TD>
                <TD>-17.447370919983996</TD>
                <TD>-10.628754</TD>
                <TD>4.565838</TD>
                <TD>-5.5614724</TD>
                <TD>12.955678</TD>
                <TD>12.163287</TD>
              </TR>
              <TR>
                <TD>5717278911884258176</TD>
                <TD>115.09192337695882</TD>
                <TD>1138.69</TD>
                <TD>-17.41606611553706</TD>
                <TD>-542.5559</TD>
                <TD>109.34396</TD>
                <TD>NaN</TD>
                <TD>12.970207</TD>
                <TD>12.747707</TD>
              </TR>
              <TR>
                <TD>5717279667799253760</TD>
                <TD>115.04547959087239</TD>
                <TD>-2.0973725</TD>
                <TD>-17.408184825633423</TD>
                <TD>0.86172307</TD>
                <TD>0.7067198</TD>
                <TD>47.9421</TD>
                <TD>13.024004</TD>
                <TD>12.438143</TD>
              </TR>
              <TR>
                <TD>5717279770877713792</TD>
                <TD>115.08180902298042</TD>
                <TD>-2.9639945</TD>
                <TD>-17.39321171722777</TD>
                <TD>-0.6608278</TD>
                <TD>0.3536838</TD>
                <TD>50.982304</TD>
                <TD>13.059916</TD>
                <TD>12.200551</TD>
              </TR>
              <TR>
                <TD>5717279289841451520</TD>
                <TD>115.02774840068207</TD>
                <TD>-6.68285</TD>
                <TD>-17.41158287028018</TD>
                <TD>-9.555653</TD>
                <TD>1.0674313</TD>
                <TD>23.113613</TD>
                <TD>13.179063</TD>
                <TD>12.7104225</TD>
              </TR>
              <TR>
                <TD>5717266473658972672</TD>
                <TD>115.1270374417545</TD>
                <TD>0.49613327</TD>
                <TD>-17.479177473210687</TD>
                <TD>-8.2364235</TD>
                <TD>5.7029996</TD>
                <TD>36.453594</TD>
                <TD>13.313252</TD>
                <TD>12.385625</TD>
              </TR>
            </TABLEDATA>
          </DATA>
        </TABLE>
      </RESOURCE>
    </VOTABLE>
}
