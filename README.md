# oefen-indicatoren

toevoeging zin in branch Aanpassing-natuurbeheer-Maarten

In deze repository kan je het beheer van de indicatoren oefenen. 
Gebruik uitsluitend oude versies (reeds gepubliceerde) van indicatoren!

## Randvoorwaarden workflow

1. Zo eenvoudig mogelijk.
1. Het is duidelijk wat de huidige "officiële" versie van de indicatoren is.
1. Je kan meerdere indicatoren gelijktijdig en onafhankelijk van elkaar bijwerken.
1. Er zijn duidelijk afspraken wie wanneer waaraan werkt zodat conflicten maximaal vermeden worden.
1. Een bijgewerkte indicator moet goedgekeurd worden voor publicatie als "officiële" versie.

## Voorgestelde workflow

1. Maak een issue aan waarin je aankondigt welke indicator je gaat bijwerken/toevoegen en geef hierin aan wie er aan meewerkt
1. Maak een nieuwe branch voor deze indicator. Pas enkel dingen aan die relevant zijn voor deze indicator.
1. Push de branch naar GitHub en maak er een pull request van. Vermeld het betrokken issue en geef aan wie de volgende is die aanpassingen moet doen.
1. Werk verder in deze branch.
1. Geef aan wanneer de indicator klaar is voor review.
1. De reviewers geven hun commentaar waarop de verantwoordelijke van de indicator de nodige aanpassingen doet.
1. Na een finale goedkeuring van de reviewers kan de beheerder de wijzingen publiceren door de pull request te mergen.

## Mappenstructuur

- source: bevat een subdirectory voor elke indicator
- publish: bevat de gerenderde versie van de indicator. Deze worden NIET onder versiebeheer geplaatst.
