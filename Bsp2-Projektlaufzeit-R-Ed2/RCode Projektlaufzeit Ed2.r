#
# R-Code zu NOT-Statistik 2. Auflage
#

# Stand 30.01.2021

## Kapitel 3 ##
#

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 51: Daten in R einlesen
# Vorbereitung: Daten-Datei "PLZ_0.CSV" lokal speichern
PLZ.0 = read.csv2(file.choose(), stringsAsFactors = TRUE)
# erstellt Daten-Container "PLZ.0"
# read.csv2() liest eine csv-Datei mit Komma als Dezimaltrenner in "PLZ.0" ein
# file.choose() öffnet den Explorer zur Dateiauswahl, ausgewählt wird "PLZ_0.CSV"
# stringsAsFactors = TRUE importiert Buchstaben/Zeichenketten als Faktoren

PLZ.0$Beginn=as.Date(PLZ.0$Beginn, "%d.%m.%Y")
PLZ.0$Ende=as.Date(PLZ.0$Ende, "%d.%m.%Y")
# PLZ.0$[Spaltenname] wählt Spalte aus
# Datumsformatierung für Spalten "Beginn" und "Ende"

str(PLZ.0) # zeigt eine Übersicht der PLZ.0-Daten und Formate an

attach(PLZ.0) # PLZ.0-Merkmale so direkt über Spaltennamen auswählbar

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 51: Balkendiagramm
# plot() zeichnet Balkendiagramm für attributive Merkmale
plot(Lieferant)
plot(Kunde)
plot(Methode)
plot(Review)
plot(Komplexität)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 51: Einzelwertdiagramme
# stripchart() zeichnet Einzelwertdiagramm
stripchart(PL, vertical=TRUE, method="stack", pch=20, main="PL")
# vertical=TRUE: Punkte werden vertikal eingezeichnet
# "stack": gleiche Werte stapeln
# pch: point character, 20 ist ein ausgefüllter Kreis
# main: setzt Titel über die Grafik
stripchart(PMa, vertical=TRUE, method="stack", pch=20, main="PMa")
stripchart(Abteilungen, vertical=TRUE, method="stack", pch=20, main="Abteilungen")
stripchart(Laufzeit, vertical=TRUE, method="stack", pch=20, main="Laufzeit")

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 52: Zeitreihendiagramme für 1 Merkmal
# plot() zeichnet Zeitreihendiagramm für variable Merkmale
plot(PL, type="b")	# type="b": both, d. h. Linien und Punkte zeichnen
plot(PMa, type="b")
plot(Abteilungen, type="b")
plot(Laufzeit, type="b")

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 52: Zeitreihendiagramm gemeinsam für Beginn und Ende
plot(Beginn, type="b", pch=20, xlab="Index")
lines(Ende, type="b", col="blue", pch=20)
# Hinzufügen der Linien und Punkte für 'Ende'
# col: color, Farbe für Linie und Punkte

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 56: Zeilennummern für Einträge finden
# which() liefert Zeilennummern mit vorgegebenen Kriterien
which(Lieferant == "Ja")
which(Kunde == "nein ")
which(Review == "eventuell" | Review == "vielleicht")
# | ist das Zeichen für ODER
which(PL > 50)
which(Laufzeit < 0)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 57: Einlesen der überarbeiteten Daten
setwd("~/Arbeit/Buch/SPM/R")

detach(PLZ.0)
# Die Merkmalsnamen im R-Speicher müssen eindeutig sein. Da die Spaltennamen in "PLZ_0.CSV" und "PLZ_1.CSV"
# identisch sind, wird zuerst die alte Daten-Datei aus dem direkten R-Speicher entfernt. Sie sind mit der
# Angabe "PLZ.0$[Spaltenname]" weiter verfügbar.

# Vorbereitung: Daten-Datei "PLZ_1.CSV" lokal speichern
PLZ.1 = read.csv2(file.choose(), stringsAsFactors = TRUE)
# erstellt Daten-Container "PLZ.1"
# eingelesen in "PLZ.1" wird jetzt die Datei "PLZ_1.CSV"

# Formatierung des Datums
PLZ.1$Beginn=as.Date(PLZ.1$Beginn, "%d.%m.%Y")
PLZ.1$Ende=as.Date(PLZ.1$Ende, "%d.%m.%Y")

str(PLZ.1) # Übersicht Einträge in "PLZ.1"
attach(PLZ.1) # Daten-Container "PLZ.1" im direkten R-Speicher verfügbar machen

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 57: Erstellen der Grafiken mit PLZ.1
# Balkendiagramme
plot(Lieferant)
plot(Kunde)
plot(Review)

# Einzelwertdiagramme
stripchart(PL, vertical=TRUE, method="stack", pch=20, main="PL")
stripchart(Laufzeit, vertical=TRUE, method="stack", pch=20, main="Laufzeit")

# Zeitreihendiagramme
plot(PL, type="b")
plot(Laufzeit, type="b")


## Kapitel 4 ##
#
# Grafiken

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 67: Boxplots
# Daten PLZ_1.CSV sind eingelesen und mit attach(PLZ.1) verfügbar gemacht

boxplot(Laufzeit ~ Lieferant, pch=8, col="lightgrey", lty=1)
# erstellt Boxplot für Laufzeit nach Lieferant
# pch=8: Extremwerte werden als Stern angezeigt
# col="lightgrey": Kästen werden hellgrau ausgefüllt
# lty=1: Linien werden durchgezogen gezeichnet
boxplot(Laufzeit ~ Kunde, pch=8, col="lightgrey", lty=1)
boxplot(Laufzeit ~ Methode, pch=8, col="lightgrey", lty=1)
boxplot(Laufzeit ~ Review, pch=8, col="lightgrey", lty=1)
boxplot(Laufzeit ~ Komplexität, pch=8, col="lightgrey", lty=1)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 69: Haupteffekte-Diagramm
# Definieren einer Haupteffekte-Diagramm-Funktion: HEdiagramm
HEdiagramm = function(x,y){
  mw = aggregate(y ~ x, data=PLZ.1, FUN=mean)
  # erstellt Variable mw
  # speichert die Mittelwerte (mw) von y je Stufe von x aus den Daten PLZ.1 über die Funktion (FUN) mean darin
  plot(mw, lty="blank", xlab=deparse(substitute(x)), ylab=deparse(substitute(y)))
  # zeichnet ein leeres Diagramm mit Achsen und Achsen-Beschriftung
  lines(mw) # fügt Verbindungslinie der Mittelwerte hinzu
  points(mw, pch=19) # zeichnet Punkte für die Mittelwerte ein
}
HEdiagramm(Lieferant,Laufzeit)
HEdiagramm(Kunde,Laufzeit)
HEdiagramm(Methode,Laufzeit)
HEdiagramm(Review,Laufzeit)
HEdiagramm(Komplexität,Laufzeit)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 72: Streudiagramm-Matrix (im Buch: Wechselwirkungs-Diagramm)
require(lattice)
# für die Streudiagramm-Matrix muss das Package "lattice" installiert sein
# Pakete in R installieren (Internetverbindung notwendig!):
# Pakete > Installiere Pakete > OK > Paketnamen in der Liste finden > OK

PLZ.1.variabel = subset(PLZ.1, select=c(PL,PMa,Abteilungen,Laufzeit))
# subset() erstellt Teilmenge aus PLZ.1, die alle vier variablen Merkmale PL, PMa, Abteilungen und Laufzeit enthält
splom(~PLZ.1.variabel)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 76: Wechselwirkungs-Diagramm
interaction.plot(Lieferant,Kunde,Laufzeit, type="b", col=1:nlevels(Kunde))
# interaction.plot ist Funktion für Wechselwirkungs-Diagramme
# hier: Lieferant auf x-Achse, Kunde für unterschiedliche Linien
# type="b": Linien und Punkte ("both") zeichnen
# col=1:nlevels(Kunde): Farben ("colors") der Linien und Punkte
interaction.plot(Lieferant,Methode,Laufzeit, type="b", col=1:nlevels(Methode))
interaction.plot(Lieferant,Review,Laufzeit, type="b", col=1:nlevels(Review))
interaction.plot(Lieferant,Komplexität,Laufzeit, type="b", col=1:nlevels(Komplexität))
interaction.plot(Kunde,Methode,Laufzeit, type="b", col=1:nlevels(Methode))
interaction.plot(Kunde,Review,Laufzeit, type="b", col=1:nlevels(Review))
interaction.plot(Kunde,Komplexität,Laufzeit, type="b", col=1:nlevels(Komplexität))
interaction.plot(Methode,Review,Laufzeit, type="b", col=1:nlevels(Review))
interaction.plot(Methode,Komplexität,Laufzeit, type="b", col=1:nlevels(Komplexität))
interaction.plot(Review,Komplexität,Laufzeit, type="b", col=1:nlevels(Komplexität))

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 81: 3D-Streudiagramme
cloud(Laufzeit ~ PL + PMa, scales = list(arrows=FALSE))
# erstellt ein 3D-Streudiagramm (Punktewolke / cloud)
# für Laufzeit (Z-Achse) mit PL (Y-Achse)  und PMa (X-Achse)
# scales = list(arrows=FALSE): Achsenbeschriftung (Skala) mit Zahlen statt Pfeilen
cloud(Laufzeit ~ PL + Abteilungen, scales = list(arrows=FALSE))
cloud(Laufzeit ~ PL + PMa, scales = list(arrows=FALSE))

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 84: Matrixplot mit attributiven Merkmalen
PLZ.1.variabel = subset(PLZ.1, select=c(PL,PMa,Abteilungen,Laufzeit))
#  subset() erstellt Teilmenge aus PLZ.1, die alle variablen Merkmale enthält

symbole = trellis.par.get("superpose.symbol")
# Eigenschaften von Gruppierungs-Symbolen in "symbole" gespeichert

splom(~PLZ.1.variabel, # Streudiagramm-Matrix für alle variablen Merkmale
	groups=Komplexität, # unterschieden nach Komplexitäts-Stufen
	key=list(columns=3,
		points=list(pch=symbole$pch[1:3], col=symbole$col[1:3]),
		text=list(levels(Komplexität)))
	# key definiert die Legende (Liste mit Vorgaben):
	# columns=3: Anzahl Legenden-Spalten über der Grafik 3
	# points=... : Vorgabe der Symbole und Farben
	# text=... : Für jedes Legenden-Symbol wird die Bezeichnung/Level von Komplexität eingefügt
	)



## Kapitel 5 ##
#
# Erstes statistisches Prozess-Modell

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 98: Erstes SPM
# Daten PLZ_1.CSV sind eingelesen und mit attach(PLZ.1) verfügbar gemacht

require(car)
# für die hier verwendeten Kontraste und Varianzanalyse (ANOVA) wird das Paket car benötigt

lm.1 = lm(Laufzeit ~ PL + PMa + Abteilungen + Lieferant + Kunde + Methode + Review + Komplexität
          + PL:PMa + PL:Abteilungen + PL:Lieferant + PL:Kunde + PL:Methode + PL:Review + PL:Komplexität
          + PMa:Abteilungen + PMa:Lieferant + PMa:Kunde + PMa:Methode + PMa:Review + PMa:Komplexität
          + Abteilungen:Lieferant + Abteilungen:Kunde + Abteilungen:Methode + Abteilungen:Review + Abteilungen:Komplexität
          + Lieferant:Kunde + Lieferant:Methode + Lieferant:Review + Lieferant:Komplexität
          + Kunde:Methode + Kunde:Review + Kunde:Komplexität
          + Methode:Review + Methode:Komplexität
          + Review:Komplexität
          + I(PL^2) + I(PMa^2),
          contrasts=list(Lieferant=contr.Sum,Kunde=contr.Sum,Methode=contr.Sum,Review=contr.Sum,Komplexität=contr.Sum),
          data=PLZ.1)
# in Container lm.1 sind die Ergebnisse des ersten SPMs gespeichert
# lm: linear model für Zielgröße Laufzeit (vor der Tilde ~)
# PL:PMa (z. B.) Wechselwirkung, durch ":" angegeben
# I(PL^2) quadratischer Effekt von PL
# contrasts: Art der Effekt-Berechnung von attributiven Merkmalen

Anova(lm.1, type=3)
# gibt eine Varianzanalyse-Tabelle mit dem Berechnungstyp 3 aus



## Kapitel 6 ##
#
# SPM vereinfachen

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 103: Auswahl Effekt-Kodierung
getOption("contrasts")
# zeigt die Einstellungen für die Kontraste an:
# erster Eintrag für Faktoren ohne Rangfolge (Voreinstellung: contr.treatment)
# zweiter Eintrag für Faktoren mit Rangfolge (Voreinstellung: contr.poly)

options(contrasts=c("contr.Sum","contr.poly"))
# setzt die Kontraste global für alle Modelle
# contr.Sum: Effekt-Kodierung für attributive Merkmale ohne Rangfolge der Stufen
# contr.poly: polynomiale Kodierung für attributive Merkmale mit Rangfolge der Stufen
getOption("contrasts")
# geänderte Kontrast-Einstellung: "contr.Sum" und "contr.poly"

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 120: Unwichtige Einflüsse ausschließen (1/2)
# Daten PLZ_1.CSV sind eingelesen
# Das SPM lm.1 ist berechnet

lm.1.step = step(lm.1, direction="both")
# step: schrittweise Auswahl wichtiger Einflüsse über AIC
# direction="both": Ausschluss und Hinzunahme in der Auswahl-Strategie

require(car) # Paket car laden

Anova(lm.1.step, type=3)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 121: Unwichtige Einflüsse ausschließen (2/2)
# Daten PLZ_1.CSV sind eingelesen

lm.2 = lm(Laufzeit ~ PL + PMa + Abteilungen + Lieferant + Kunde + Methode + Review + Komplexität
          + I(PMa^2)
          + PL:PMa
          + Abteilungen:Methode
          + Lieferant:Kunde
          + Kunde:Komplexität
          + Methode:Review, 
          data = PLZ.1)

Anova(lm.2, type=3)



## Kapitel 7 ##
#
# Modell-Qualität prüfen

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 133: Modell-Qualitäts-Kennzahlen R², R²(prog) und S
# lm.2 wurde berechnet

SST = sum((Laufzeit-mean(Laufzeit, na.rm=TRUE))^2)
# berechnet SST und speichert Wert in "SST"
SSE = sum(resid(lm.2)^2)
# berechnet SSE und speichert Wert in "SSE"
R2 = 1-SSE/SST # berechnet R² und speichert Wert in "R2"
R2 # zeigt Wert von "R2" an, hier: 0.8744451 = 87,44%

PRESS = sum((resid(lm.2)/(1-hatvalues(lm.2)))^2)
# berechnet PRESS und speichert Wert in "PRESS"
PRESS # zeigt Wert von "PRESS" an
R2.prog = 1-PRESS/SST # berechnet R²_prog und speichert Wert in "R2.prog"
R2.prog # zeigt Wert von "R2.prog" an, hier: 0.8366248 = 83,66%

S.spm = sqrt(SSE/df.residual(lm.2))
# berechnet Modell-Standardabweichung S und speichert Wert in "S.spm"
# df.residual ermittelt die Freiheitsgrade der Residuen, hier: n-k=115
S.spm # zeigt Wert von "S.spm" an, hier: 8.533396 = 8,53 Tage

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 141: Residuendiagramme: Wahrscheinlichkeitsnetze
# lm.2 wurde berechnet

qqnorm(residuals(lm.2), xlab="Normalverteilungs-Quantile", ylab="Residuen",
	main="Wahrscheinlichkeitsnetz", pch=20)
qqline(residuals(lm.2), col="blue")
# qqnorm zeichnet Wahrscheinlichkeitsnetz für Normalverteilung
# residuals liefert Residuen
# xlab, ylab, main: Beschriftung x-Achse, y-Achse und Titel
# qqline zeichnet Ideallinie (hier: blau)

qqnorm(rstandard(lm.2), xlab="Normalverteilungs-Quantile", ylab="standardisierte Residuen",
	main="W.-Netz", pch=20)
qqline(rstandard(lm.2), col="blue")
# rstandard liefert standardisierte Residuen

qqnorm(rstudent(lm.2), xlab="Normalverteilungs-Quantile", ylab="studentisierte Residuen",
	main="W.-Netz", pch=20)
qqline(rstudent(lm.2), col="blue")
# rstudent liefert studentisierte Residuen

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 142: Residuendiagramme: Anpassungen vs. Residuen und Beobachtungsnr. vs. Residuen
# lm.2 wurde berechnet

plot(fitted(lm.2), residuals(lm.2), xlab="Angepasste Werte für Laufzeit", ylab="Residuen", main="Angepasste Werte vs. Residuen", pch=20)
abline(h=0, col="blue")
# fitted liefert die angepassten Werte für die Laufzeit
# abline zeichnet eine Referenzlinie ein, h: horizontal

plot(residuals(lm.2), type="b", xlab="Beobachtungsnummer", ylab="Residuen", main="Beobachtungsnummer vs. Residuen", pch=20)
abline(h=0, col="blue")
# type="b" zeichnet Punkte und Linien ("both")

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 143: Residuendiagramme: Auffällige Residuen finden
# lm.2 wurde berechnet

PLZ.1.SPM = cbind(PLZ.1,Fit=fitted(lm.2),Resi=residuals(lm.2))
# cbind: column-bind, hängt Spalten aneinander
# erzeugt Container PLZ.1.SPM mit den Daten PLZ.1 sowie den
# Anpassungen (Fit) und Residuen (Resi)

PLZ.1.SPM[which(residuals(lm.2) < -20),]
# zeigt die Zeile(n) aus PLZ.1.SPM an, für die die Residuen kleiner -20 sind

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 158: Modell-Qualitäts-Kennzahlen: Test auf Normalverteilung für Residuen
# lm.2 wurde berechnet

require(nortest)
# für den Anderson-Darling-Test wird das Paket "nortest" benötigt
ad.test(residuals(lm.2))
# berechnet den Anderson-Darling-Test auf Normalverteilung (ad.test) und gibt die Werte aus



## Kapitel 8 ##
#
# Modell-Interpretation und Prognose

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 166: Effektediagramme
# lm.2 wurde berechnet

require(effects) # für die Effektdiagramme muss das Paket "effects" installiert sein

# Haupteffekte-Diagramme mit großem E bei Effect
plot(Effect("PL", lm.2, ylim=c(91,156)))
	# ylim gibt Wertebereich der y-Achse an
plot(Effect("PMa", lm.2, ylim=c(91,156)))
plot(Effect("Abteilungen", lm.2, ylim=c(91,156)))
plot(Effect("Lieferant", lm.2, ylim=c(91,156)))
plot(Effect("Kunde", lm.2, ylim=c(91,156)))
plot(Effect("Methode", lm.2, ylim=c(91,156)))
plot(Effect("Review", lm., ylim=c(91,156),2))
plot(Effect("Komplexität", lm.2, ylim=c(91,156)))


# Wechselwirkungsdiagramme mit kleinem e bei effect
plot(effect("PL:PMa", lm.2, ylim=c(72,168)))
plot(effect("Abteilungen:Methode", lm.2, ylim=c(72,168)))
plot(effect("Lieferant:Kunde", lm.2, ylim=c(72,168)))
plot(effect("Kunde:Komplexität", lm.2, ylim=c(72,168)))
plot(effect("Methode:Review", lm.2, ylim=c(72,168)))

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 168: Quantile und Auswahl von Projekten
PLZ.1[PL >= quantile(PL, .75) 
      & PMa >= quantile(PMa, .75) 
      & Laufzeit >= quantile(Laufzeit, .75),]
# [... ,] gibt Auswahlkriterium an
# quantile(x, .75) berechnet 75%-Quantil von x
# Auswahl PL >= Q_75(PL)
# und (&) PMa >= Q_75(PMa)
# und (&) Laufzeit >= Q_75(Laufzeit)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 174: Kontur- und Wirkungsflächendiagramme Laufzeit
# lm.2 wurde berechnet

require(lattice) # Pakte lattice laden

# Gitter-Stützpunkte berechnen
PL.p = seq(min(PL), max(PL), length.out=100)
PMa.p = seq(min(PMa), max(PMa), length.out=100)
# seq() erstellt eine Sequenz von Werten
Abteil.p = c(1,3,6)
punkte = list(PL=PL.p, PMa=PMa.p, Abteilungen=Abteil.p)
# list() erstellt eine Liste
gitter = expand.grid(punkte)
# erstellt das Gitter für die Stützpunkte

gitter.niedrig = cbind(gitter,
	data.frame(Lieferant="nein", Kunde="nein",
	Methode="SiSi", Review="nein",
	Komplexität="niedrig"))
# data.frame() erstellt eine Datenstruktur
gitter.niedrig[, "Prognose"] = c(predict(lm.2, gitter.niedrig))
# mit [,...] wird an den vorhandenen data.frame eine Spalte angefügt
# predict(modell,daten) berechnet Prognosen für "daten", hier für gitter.niedrig

gitter.hoch = cbind(gitter,
	data.frame(Lieferant="ja", Kunde="ja",
	Methode="PDCA", Review="ja",
	Komplexität="hoch"))
gitter.hoch[, "Prognose"] = c(predict(lm.2, gitter.hoch))

# Konturdiagramme
contourplot(Prognose ~ PL*PMa | Abteilungen,
	data=gitter.niedrig, main="Kontur niedrig",
	region=TRUE,cuts=10^3, contour=FALSE, 
	col.regions=rainbow(2000))
# contourplot() erstellt das Konturdiagramm
# Prognose ~ PL*PMa | Abteilungen: Höhenlinien aus "Prognose", 
# x- und y-Achse PMa und PL,
# unterschiedliche Grafiken je Wert in "Abteilungen"

contourplot(Prognose ~ PL*PMa | Abteilungen,
	data=gitter.hoch, main="Kontur hoch",
	region=TRUE,cuts=10^3, contour=FALSE,
	col.regions=rainbow(2000))

# Wirkungsflächendiagramme
wireframe(Prognose ~ PL*PMa | Abteilungen,
	data=gitter.niedrig, main="Wirkungsfläche niedrig",
	zlab="", colorkey=FALSE, shade=TRUE, drape=TRUE,
	scales=list(arrows=FALSE), light.source = c(0,2,2))
# wireframe() erstellt das Wirkungsflächendiagramm
# Achsen und Grafiken wie in countourplot (s. o.)

wireframe(Prognose ~ PL*PMa | Abteilungen,
	data=gitter.hoch, main="Wirkungsfläche hoch", 
	zlab="", colorkey=FALSE, shade=TRUE, drape=TRUE,
	scales=list(arrows=FALSE), light.source = c(0,2,2))

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 187: Koeffizienten des vereinfachten SPMs ausgeben
# lm.2 wurde berechnet

round(lm.2$coef,2) # zeigt Koeffizienten von lm.2 an

str(PLZ.1) # zeigt Beschreibung der Daten inkl. Stufen von attributiven Einflussgrößen an

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 195 Prognosen für Laufzeit
# lm.2 wurde berechnet

# Einstellwerte vorgeben
Daten.neu = data.frame(Lieferant="ja", Kunde="nein",
	Methode="PDCA", Review="ja",
	Komplexität=c("hoch","mittel","niedrig"), 
	PL=10, PMa=5, Abteilungen=2)
# data.frame() erstellt eine Datenstruktur
# Spalten werden aufgefüllt, so dass alle Spalten dieselbe Länge haben

# Prognose berechnen
Prognose=predict(lm.2, Daten.neu)

cbind(Daten.neu,Prognose)
# cbind() verbindet Spaltenweise (column-bind)
# Ergebnis: Anzeige der Einstellwerte und Prognosen

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 198: Vertrauens- und Prognosebereiche für Laufzeit
# lm.2 wurde berechnet

# Einstellwerte vorgeben
Daten.neu = data.frame(Lieferant="ja", Kunde="nein",
	Methode="PDCA", Review="ja",
	Komplexität=c("hoch","mittel","niedrig"), 
	PL=10, PMa=5, Abteilungen=2)

# Prognose berechnen
Prognose=round(predict(lm.2, Daten.neu),2)
# round() rundet Werte, hier auf 2 Nachkommastellen

# Obere Grenze 98% Vertrauensbereich berechnen
OGW.KI=round(predict(lm.2, Daten.neu, interval="confidence", level=0.98)[,3],2)
# [,3] wählt dritte Spalte aus, die die OGW angibt

# Obere Grenze 98% Prognosebereiche berechnen
OGW.PI=round(predict(lm.2, Daten.neu, interval="prediction", level=0.98)[,3],2)

cbind(Prognose,OGW.KI,OGW.PI)
# cbind() verbindet Spaltenweise (column-bind)
# Ergebnis: Anzeige der Prognosen und obere Streubereichsgrenzen



## Kapitel 9 ##
#
# Prozess-Simulation

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 206:  Anzahl Werte für Lieferant
# Daten PLZ_1.CSV sind eingelesen und mit attach(PLZ_1.CSV) verfügbar gemacht
table(Lieferant)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 207: Monte Carlo-Simulation
# lm.2 wurde berechnet

Simu=10^4 # Anzahl Simulationen in "Simu" speichern

simu.attr=function(Merkmal,Simu) {
	sample(rep(levels(Merkmal),rmultinom(1,Simu,prob=table(Merkmal))),Simu)
	}
# Funktion "simu.attr" für die Einstellwerte der attributiven Merkmale:
# 2 Werte: Merkmal und Anzahl (Simu)
# sample() zieht eine Stichprobe aus den Werten
# rep(a,b) wiederholt den Eintrag "a" b Mal
# levels(Merkmals) Stufen des Merkmals
# rmultinom: Zufallszahlen der Multinomialverteilung (2 oder mehr Gruppen im Merkmal)
# rmultinom(a,b,c) erzeugt a Mal Zufallswerte, je Durchlauf b Werte mit den Wahrscheinlichkeiten c je Stufe des Merkmals
# prog=table(Merkmal) gibt die Anzahl Werte je Stufe wieder, die in rmultinom automatisch in Anteile umgewandelt werden

sim.Lief=simu.attr(Lieferant,Simu)
# speichert Zufallswerte für Lieferant in sim.Lief
sim.Kund=simu.attr(Kunde,Simu)
sim.Meth=simu.attr(Methode,Simu)
sim.Rev=simu.attr(Review,Simu)
sim.Kompl=simu.attr(Komplexität,Simu)

sim.PL=abs(rnorm(Simu, mean(PL), sd(PL)))
# speichert Zufallswerte für PL in sim.PL
# abs() berechnet den Absolutwert
sim.PMa=ceiling(abs(rnorm(Simu, mean(PMa), sd(PMa))))
# ceiling berechnet die nächstgrößere ganze Zahl
sim.Abt=ceiling(abs(rnorm(Simu, mean(Abteilungen),sd(Abteilungen))))

# Zusammenstellung der Daten für die Monte Carlo-Simulation
sim.daten = data.frame(Lieferant=sim.Lief,Kunde=sim.Kund,Methode=sim.Meth,Review=sim.Rev,
	Komplexität=sim.Kompl,PL=sim.PL,PMa=sim.PMa,Abteilungen=sim.Abt)

# Berechnung der simulierten Laufzeit
Laufzeit.sim=predict(lm.2,sim.daten)
MC.daten=cbind(sim.daten,Laufzeit.sim)

# Boxplot simulierte Laufzeit nach Review, Methode und Komplexität
boxplot(Laufzeit.sim ~ Review+Methode+Komplexität, pch=8, col="lightgrey", lty=1, 
        axes=FALSE, xlab="", ylab="", data=MC.daten)
mtext(side=1, line=3.5, at=c(2.5,6.5,10.5), levels(Komplexität))
mtext(side=1, line=3.5, at=-2.0, adj=c(0), "Komplexität")
mtext(side=1, line=2, at=seq(1.5,11.5,by=2), levels(Methode))
mtext(side=1, line=2, at=-2.0, adj=c(0), "Methode")
mtext(side=1, line=0.5, at=1:12, levels(Review))
mtext(side=1, line=0.5, at=-2.0, adj=c(0), "Review")
axis(1, at=1:12, labels=NA)
axis(2, las=2)
box()


# Boxplot simulierte Laufzeit nach Lieferant, Kunde und Komplexität
boxplot(Laufzeit.sim ~ Lieferant+Kunde+Komplexität, pch=8, col="lightgrey", lty=1, 
        axes=FALSE, xlab="", ylab="", data=MC.daten)
mtext(side=1, line=3.5, at=c(2.5,6.5,10.5), levels(Komplexität))
mtext(side=1, line=3.5, at=-2.0, adj=c(0), "Komplexität")
mtext(side=1, line=2, at=seq(1.5,11.5,by=2), levels(Kunde))
mtext(side=1, line=2, at=-2.0, adj=c(0), "Kunde")
mtext(side=1, line=0.5, at=1:12, levels(Lieferant))
mtext(side=1, line=0.5, at=-2.0, adj=c(0), "Lieferant")
axis(1, at=1:12, labels=NA)
axis(2, las=2)
box()


#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 208: 95%-Prognosebereich
# Laufzeit.sim wurde berechnet

# Berechnung und Ausgabe der Quantilwerte
PI = quantile(Laufzeit.sim, prob=c(0.025,0.975))
PI


#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 217: 95%-Streubereich für die Anpassungsgüte R²
#lm.2 wurde berechnet

require(car) # Paket car laden

BTD.Laufzeit=Boot(lm.2, 
	f = function(mod){summary.lm(mod)$r.squared},
	R=10^4)
# Boot(daten,funktion,anzahl,...) ist Bootstrapping-Funktion
# f gibt die Funktion für die Kennzahl R² an
# hier: R² aus der Zusammenfassung des Modells 
# R: Anzahl Simulationsdurchläufe

confint(BTD.Laufzeit, level=.95, type="perc")
# berechnet den Streubereich von BTD.Laufzeit
# level Abdeckbereich
# perc: Perzentil- bzw. Quantil-Methode

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 218: Boxplot
# Simulierte Anpassungsgüten BTD.Laufzeit sind berechnet
boxplot(BTD.Laufzeit$t, pch=8, col="lightgrey", lty=1)
# BTD.Laufzeit$t: simulierte BTD.Laufzeit-Anpassungsgüten
abline(h=c(0.8327,0.9262,0.8744), col="red")
# zeichnet drei horizontale Linien ein

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 218: Zeitreihendiagramm
# Simulierte Anpassungsgüten BTD.Laufzeit sind berechnet
plot(BTD.Laufzeit$t)
# BTD.Laufzeit$t: simulierte BTD.Laufzeit-Anpassungsgüten
abline(h=c(0.8327,0.9262,0.8744), col="red")
# zeichnet drei horizontale Linien ein

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 225: 95%-Streubereich für die Anpassungsgüte R²
# lm.2 wurde berechnet

require(car) # Paket car laden

BTR.Laufzeit=Boot(lm.2, 
	f = function(mod){summary.lm(mod)$r.squared},
	R=10^4,
	method="residual")
# Boot(daten,funktion,anzahl,...) ist Bootstrapping-Funktion
# f gibt die Funktion für die Kennzahl R² an
# hier: R² aus der Zusammenfassung des Modells 
# R: Anzahl Simulationsdurchläufe
# method wählt die Bootstrap-Methode, "residual": Residuen wie in (31)

confint(BTR.Laufzeit, level=.95, type="perc")
# berechnet den Streubereich von BTR.Laufzeit
# level Abdeckbereich
# perc: Perzentil- bzw. Quantil-Methode

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 225: Boxplot
# Simulierte Anpassungsgüten BTR.Laufzeit sind berechnet
boxplot(BTR.Laufzeit$t, pch=8, col="lightgrey", lty=1)
# BTR.Laufzeit$t: simulierte BTR.Laufzeit-Anpassungsgüten
abline(h=c(0.8430,0.9083,0.8744), col="red")
# zeichnet drei horizontale Linien ein

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 226: Zeitreihendiagramm
# Simulierte Anpassungsgüten BTR.Laufzeit sind berechnet
plot(BTR.Laufzeit$t)
# BTR.Laufzeit$t: simulierte BTR.Laufzeit-Anpassungsgüten
abline(h=c(0.8430,0.9083,0.8744), col="red")
# zeichnet drei horizontale Linien ein



## Kapitel 10 ##
#
# Optimierung und Nachweis

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 236: Zielgrößenoptimierung
# lm.2 wurde berechnet

# Zielgrößenoptimierung
# Wunschfunktion d*_min als Funktion "d.min2" definieren
d.min2=function(y,OG,Ziel,w) {
  ifelse(y>OG, 0, ((OG-y)/(OG-Ziel))^w*100 ) }
# ifelse(Bedingung, A, B): prüft Bedingung für jedes Element
# und gibt A zurück wenn wahr und B wenn falsch

# Rasterwerte erstellen
d.PL=seq(min(PL),max(PL),length=100)
# seq() erstellt eine Sequenz von Werten
# min() liefert das Minimum
# max() liefert das Maximum
# length gibt an aus wie vielen Werte die Sequenz besteht
d.PMa=seq(min(PMa),max(PMa))
d.Abteilungen=seq(min(Abteilungen),max(Abteilungen))
d.Lieferant=levels(Lieferant)
# levels() gibt die Kategorien eines attributiven Merkmals aus
d.Kunde=levels(Kunde)
d.Methode=levels(Methode)
d.Review=levels(Review)
d.Komplexität=levels(Komplexität)

# Rasterwerte in einer Liste speichern
Raster.Liste = list(PL=d.PL, PMa=d.PMa,
	Abteilungen=d.Abteilungen, Lieferant=d.Lieferant,
	Kunde=d.Kunde, Methode=d.Methode,
	Review=d.Review, Komplexität=d.Komplexität)

# alle Kombinationen erzeugen
Raster = expand.grid(Raster.Liste)

# Prognosewerte berechnen und in Spalte "Prognose" anhängen
Raster[, "Prognose"] = c(predict(lm.2, Raster))
# Werte von d*_min berechnen und in Spalte "dmin2" anhängen
Raster[, "dmin2"] = d.min2(Raster$Prognose, OG=50, Ziel=25, w=1)

# Auswahl kurzer Projekte
kurze.Projekte = subset(Raster, Raster$dmin2 > 0)
#  subset() erstellt Auswahl nach Kriterium "Raster-Werte d*_min > 0"
dim(Raster)
dim(kurze.Projekte)
# dim() liefert die Dimensionen, d. h. Anzahl Zeilen (1. Wert) und Spalten (2. Wert)
summary(kurze.Projekte)
# summary() liefert Kennzahlen
Raster[Raster$dmin2==max(Raster$dmin2),]
# zeigt die Einstellungen an mit denen dmin2 maximal ist


#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 253: Finden der Kombinationen mit obere Prognosegrenze > 200 Tage
# lm.2 wurde berechnet

# Worst-Case-Wunschfunktion d*_max als Funktion "d.max" definieren
d.max=function(y,UG,Ziel,w) {
  ifelse(y<UG, 0, 
		ifelse(y>Ziel, 100, ((y-UG)/(Ziel-UG))^w*100 ))
  }
# ifelse(Bedingung, A, B): prüft Bedingung für jedes Element
# und gibt A zurück wenn wahr und B wenn falsch

# Rasterwerte erstellen
d.PL=seq(min(PL),max(PL),length=100)
d.PMa=seq(min(PMa),max(PMa))
d.Abteilungen=seq(min(Abteilungen),max(Abteilungen))
d.Lieferant=levels(Lieferant)
d.Kunde=levels(Kunde)
d.Methode=levels(Methode)
d.Review=levels(Review)
d.Komplexität=levels(Komplexität)

# Rasterwerte in einer Liste speichern
Raster.Liste = list(PL=d.PL, PMa=d.PMa,
	Abteilungen=d.Abteilungen, Lieferant=d.Lieferant,
	Kunde=d.Kunde, Methode=d.Methode,
	Review=d.Review, Komplexität=d.Komplexität)

# alle Kombinationen erzeugen
Raster = expand.grid(Raster.Liste)

# Obere 99%-Prognosegrenze "OPG" berechnen
OPG = as.data.frame(predict(lm.2, Raster,
	interval="prediction", level=0.98))$upr
# predict(., interval="'prediction"') berechnet zweiseitige Prognosegrenzen
# level=0.98 Anteil innerhalb des zweiseitig begrenzten Prognoseintervalls an
# bei 98% innerhalb liegen 2% außerhalb, mit 1% > OPG
# $upr wählt obere Prognosegrenze (upper prediction limit) für OPG aus

# und in Spalte "OPG" anhängen
Raster[, "Prognosegrenze"] = OPG

# Projekte mit OPG>200 in "lange.projekte" speichern
lange.projekte=subset(Raster, Raster$Prognosegrenze > 200)
summary(lange.projekte)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 254 Einschränkung der Einstellkombinationen auf obere Prognosegrenze < 200 Tage
# Der Datensatz "lange.projekte" wurde erstellt

# Matrixplot für die variablen Merkmale PL, PMa und Abteilungen
require(lattice) # Paket lattice laden

lange.projekte.variabel = subset(lange.projekte, select=c(PL,PMa, Abteilungen))
# Auswahl der variablen Merkmale aus "lange.projekte"
splom (~lange.projekte.variabel) 
# Matrixplot für alle variablen Einflüsse
# kaum sicherer Bereich identifizierbar

# Kombinationen der attributiven Merkmale erstellen
werte.attributiv = list(Lieferant=d.Lieferant, Kunde=d.Kunde,
                        Methode=d.Methode, Review=d.Review, 
                        Komplexität=d.Komplexität)
kombi = expand.grid(werte.attributiv)

# Platzhalter "Anzahl" definieren, der so viele Stellen hat wie es Kombinationsmöglichkeiten der attributiven Merkmale gibt (hier: 48)
Anzahl=numeric(nrow(kombi))
# nrow() zählt Anzahl Zeilen (number of rows)

# Für alle Kombinationsmöglichkeiten in "kombi" zählen, wie oft (Anzahl) OPG > 200~Tage im Datensatz "lange.projekte" ist
for(i in 1:nrow(kombi)){
	Anzahl[i] = nrow(subset(lange.projekte, 
		(Lieferant==kombi$Lieferant[i])
		& (Kunde==kombi$Kunde[i]) 
		& (Methode==kombi$Methode[i])
		& (Review==kombi$Review[i])
		& (Komplexität==kombi$Komplexität[i])))
	}

# Anfügen der Spalte "Anzahl" an "kombi" und speichern als Datensatz "Häufigkeit"
Häufigkeit=data.frame(cbind(kombi,Anzahl))
# data.frame() erstellt Datenstruktur
# cbind() verbindet Spalten (column bind)

# Auswahl der Kombinationen, in denen nie OPG > 200 bzw. Anzahl(OPG>200)=0 ist
Sicher=subset(Häufigkeit, Anzahl==0)

summary(Sicher)
# Kennzahlen für Datensatz "Sicher"
# kein attributives Merkmal hat bei Projekten mit OPG <= 200 immer dieselbe Einstellung
# tendenziell Merkmal Komplexität am eindeutigsten

# Kombinationen der attributiven Merkmale mit OPG kleiner 200~Tage je Komplexitäts-Level
subset(Sicher, Komplexität=="hoch")
subset(Sicher, Komplexität=="mittel")
subset(Sicher, Komplexität=="niedrig")	



## Kapitel 11 ##
#
# Toleranzen


#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 265: Test auf Normalverteilung und Wahrscheinlichkeitsnetz
# Daten PLZ_1.CSV sind eingelesen und mit attach(PLZ_1.CSV) verfügbar gemacht

# Test auf Normalverteilung (Anderson-Darling)
require(nortest) # Paket nortest laden
ad.test(Laufzeit)
# berechnet den Anderson-Darling-Test auf Normalverteilung (ad.test) und gibt die Werte aus

# Wahrscheinlichkeitsnetz
qqnorm(Laufzeit, xlab="Normalverteilungs-Quantile",
	ylab="Laufzeit", main="Wahrscheinlichkeitsnetz", pch=20)
qqline(Laufzeit, col="blue")
# qqnorm zeichnet Wahrscheinlichkeitsnetz für Normalverteilung
# xlab, ylab, main: Beschriftung x-Achse, y-Achse und Titel
# qqline zeichnet Ideallinie (hier: blau)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 271: Toleranzgrenze ohne Normalverteilung
# Daten PLZ_1.CSV sind eingelesen

require(tolerance) # für die Toleranzrechnung muss das Package "tolerance" installiert sein

nptol.int(Laufzeit, alpha=0.05, P=0.99, side=1)
# nptol.int() berechnet verteilungsfreie Toleranzintervalle
# alpha=0.05 entspricht Konfidenz- oder Vertrauensniveau von 95%
# P=0.99 Abdeckung der Laufzeit (99%)
# side=1 berechnet einseitige Toleranzgrenzen

distfree.est(n = length(Laufzeit), P = 0.99, side = 1)
# berechnet die erreichte Konfidenz für 99% Abdeckung bei n=132 Werten

distfree.est(n = length(Laufzeit), alpha=0.05, side = 1)
# berechnet die erreichbare Abdeckung bei n=132 Werten und 95% Konfidenz

distfree.est(P=0.99, alpha=0.05, side = 1)
# berechnet die Mindestanzahl Messwerte für 99% Abdeckung und 95% Konfidenz

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 277: Monte Carlo-Toleranz
# Daten PLZ_1.CSV sind eingelesen und mit attach(PLZ_1.CSV) verfügbar gemacht
# lm.2 wurde berechnet 

Simu=10^4 # Anzahl Simulationen in "Simu" speichern

# Zusammenstellung der Daten für die Monte Carlo-Simulation
sim.PL=abs(rnorm(Simu, mean(PL), sd(PL)))
sim.PMa=ceiling(abs(rnorm(Simu, mean(PMa), sd(PMa))))
sim.Abt=ceiling(abs(rnorm(Simu, mean(Abteilungen),sd(Abteilungen))))
sim.daten = data.frame(Lieferant="ja",Kunde="ja",Methode="SiSi",Review="ja",Komplexität="hoch",PL=sim.PL,PMa=sim.PMa,Abteilungen=sim.Abt)

# Berechnung der simulierten Projektdurchlaufzeiten
Laufzeit.sim=predict(lm.2,sim.daten)
MC.daten=cbind(sim.daten,Laufzeit.sim)

# Boxplot simulierte Laufzeit
boxplot(Laufzeit.sim, pch=8, col="lightgrey", lty=1, data=MC.daten)

# Test auf Normalverteilung (Anderson-Darling) und Wahrscheinlichkeitsnetz
require(nortest) # Paket nortest laden
ad.test(Laufzeit.sim)

qqnorm(Laufzeit, xlab="Normalverteilungs-Quantile",
	ylab="Laufzeit", main="Wahrscheinlichkeitsnetz", pch=20)
qqline(Laufzeit, col="blue")

# Berechnen der 99%-Toleranzgrenze
require(tolerance) # Paket tolerance laden
 
nptol.int(Laufzeit.sim, alpha=0.05, P=0.99, side=1)
distfree.est(n = length(Laufzeit.sim), P = 0.99, side = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++#
# S. 291: Bootstrap-Toleranz
# Daten PLZ_1.CSV sind eingelesen und mit attach(PLZ_1.CSV) verfügbar gemacht
# lm.2 wurde berechnet 

# Einstellwerte für Prognose
x.p = data.frame(Lieferant="ja",Kunde="ja",Methode="SiSi",
	Review="ja",Komplexität="hoch", PL=median(PL),PMa=median(PMa),
	Abteilungen=median(Abteilungen))

# Prognosewert ydach_p
y.p = predict(lm.2,x.p)

# modifizierte Residuen
ri = lm.2$res/sqrt(1-lm.influence(lm.2)$hat) - mean(lm.2$res/sqrt(1-lm.influence(lm.2)$hat))
# lm.2$res Residuen von lm.2
# sqrt() Wurzelfunktion
# lm.influence(lm.2)$hat Hebelwirkungen von lm.2
# mean() Mittelwert

# Einstellwerte für SPM
x.werte = subset(PLZ.1, select=c(PL, PMa, Abteilungen, Lieferant, Kunde, Methode, Review, Komplexität))
# Auswahl der Spalten mit Einstellwerten

Simu=10^4 # Anzahl Simulationen in "Simu" speichern

# Container für "delta.stern" und "y.sim" definieren
delta.stern = NULL
y.sim = NULL

# Werte "delta.stern" und "y.sim" berechnen
for(i in 1:Simu){  
	e.stern = sample(ri, size=nrow(PLZ.1), replace=TRUE)
	y.stern = lm.2$fit + e.stern
	lm.2.stern = lm(y.stern ~ PL + PMa + Abteilungen + Lieferant 
		+ Kunde + Methode + Review + Komplexität  + I(PMa^2) 
		+ PL:PMa + Abteilungen:Methode + Lieferant:Kunde 
		+ Kunde:Komplexität + Methode:Review,
		data=cbind(x.werte,y.stern))
	y.p.stern = predict(lm.2.stern,x.p)
	e.p.stern = sample(ri, size=nrow(x.p), replace=TRUE)
	delta.stern[i] = y.p.stern - (y.p+e.p.stern)
	# i-ter Wert für delta.stern
	y.sim[i] = y.p + delta.stern[i]
	# i-ter simulierte Bootstrap-Prognosewert
	}

# Boxplot simulierte Laufzeit
boxplot(y.sim, pch=8, col="lightgrey", lty=1)

# Test auf Normalverteilung (Anderson-Darling) und Wahrscheinlichkeitsnetz
require(nortest) # Paket nortest laden
ad.test(y.sim)

qqnorm(y.sim, xlab="Normalverteilungs-Quantile",
	ylab="Laufzeit", main="Wahrscheinlichkeitsnetz", pch=20)
qqline(y.sim, col="blue")

# Berechnen der 99%-Toleranzgrenze
require(tolerance) # Paket tolerance laden
normtol.int(y.sim, alpha=0.05, P=0.99, side=1)
# normtol.int() berechnet Toleranzgrenzen für normalverteilte Werte

