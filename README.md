# General Application for Groundwater Analytics

## Introduction

This application provides various tools to assist with the analysis of groundwater data. These include data screening tables against standards for selected chemicals, interactive charts, automated formatted statistical summaries as well as automated report charts.

This is a generalised version of the [Fort Hall Mine Landfill Shiny app](https://github.com/cdmsmithinc/1001406_bannock). 

## The server

The application lives on a VM on AWS: AW02PAPPC004

The Shiny Server is hosted on an Ubuntu 22.04 VM hosted on AWS in the US. The Shiny Server is running R 4.1.2.

To update on the server:

* login to AW02PAPPC004 (the Host Name) using PuTTY - port 22, SSH connection type
* login using your CDMSmith credentials
* login as a super user: sudo -i then enter your password again
* navigate to the project folder: `cd /srv/shiny-server/dev/1001406_general_groundwater` for the dev version and `cd /srv/shiny-server/1001406_general_groundwater` for the production version (which doesn't exist yet)
* make sure you're in the right branch, then pull the updates, e.g. `git pull origin dev_v0.1`
* Access to GitHub is via SSH

## Development and production versions

The **development** version of the application lives on a Shiny Server here: <http://aw02pappc004:3838/dev/1001406_general_groundwater/>

The **production** version can be found here: <http://aw02pappc004:3838/1001406_general_groundwater/>

The development version will be where changes are tested. These will flow in to the production version.

## Project contacts

* Emma Ehret, Environmental Engineer – Application Design, Technical Direction, User Experience, Project Implementation – CDM Smith, ehretele@cdmsmith.com  
* William Lai, Senior Data Scientist – Application Development, Programmer – CDM Smith Australia, laiw@cdmsmith.com  
* Hannah Rolston, Environmental Engineer – User Experience, Project Implementation – CDM Smith, rolstonhm@cdmsmith.com  

## Version information

see [news.md](/news.md) file

## Template sheet protection

The Excel upload templates can be found in the `upload_template` folder. The Excel template has a password for the first two sheets. Password is 'gaga'.