R We The People?
----------------


An R Package for working with the White House's We The People API.


The package is currently focused on loading petition data,
running ad-hoc analysis and exploratory visualizations.

Usage
-----

    install.packages('wethepeople')

    library(wethepeople)
    example(wethepeople)

    # Load petitions
    client <- WeThePeopleClient()
    petitions <- client$petitions()

    # Make some eye candy
    plot_issues_over_time(petitions)
    plot_issues_pending_response(petitions)


Examples
--------

There following images were generated using the package examples.

#### Petition Issues Over Time
![issues](examples/issues_over_time.png)
![issues](examples/issues_pending.png)

#### Status by Creation Date
![issues](examples/status_by_creation.png)


#### All Petitions' Title Word Cloud
![title wordcloud](examples/title_wordcloud.png)

#### All Petitions' Body Word Cloud
![body wordcloud](examples/body_wordcloud.png)

#### Single Petition Body and Title Word Cloud

![body wordcloud](examples/petition_wordcloud_4e7b3ea711fb9c3d7a000004.png)

![body wordcloud](examples/petition_wordcloud_4e7b3f188d8c37d875000004.png)

![body wordcloud](examples/petition_wordcloud_4e7b70294bd5044b0c00000f.png)

![body wordcloud](examples/petition_wordcloud_4e7ca6422ee8d0fa79000096.png)

