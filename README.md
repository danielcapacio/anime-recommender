# Anime Recommender Application
CPSC 312 Project 1: Functional Programming

### To Run:
In root directory, type
```
cabal run
```
### What is the problem?
We want to solve the problem of a person’s indecisiveness when choosing an anime to watch. Quick, and on-the-spot recommendations would be ideal for our user. Therefore, our application will take into consideration the user’s top five favourite genres as well as their favourite anime to recommend our top three choices to watch next.

### What is the "something extra"?
Our application has two special aspects that make it stand out. First, we implemented a GUI for the user to input their genre preferences and also to select a visual representation of their favourite anime. This approach greatly improves the usability of our application, diminishing the chances of input error and boosting simplicity. The second stand out feature we implemented was creating an anime database using the Persistent package. We make calls to a public [API](https://docs.api.jikan.moe) to collect data on the top 100 anime. This information is then parsed from the JSON format, placed in the database, and queried on using Persistent to come up with the recommendation.

### What did we learn from doing this?
Parsing a JSON using functional programming is difficult, but it offers a lot of control over what is parsed and how we store the parsed result. It is also possible to interact with a database using Haskell, and the strongly typed nature of Haskell makes the database quite resilient to unexpected behaviour. Persistent is a well documented and detailed package to interact with a database. Functional programming was very useful for creating the getGenres/getThemes/etc functions because it makes it easy to define new functions in terms of an already existing general function (in our case getField). This greatly improved the readability and conciseness of our code.

We used the Haskell package, [`threepenny-gui`](https://hackage.haskell.org/package/threepenny-gui), for our user interface. We wanted a GUI that would not only be sleek and appealing, but intuitive in design as well. Thus, we came to a conclusion that a web browser application would satisfy these requirements by allowing the user to:

- __intuitively__ click buttons of their preferred genres and
- __visually__ see the posters, while having the ability to click their preferred one
Overall, this package provided us with a straightforward implementation route for rendering HTML DOM (document object model) elements.

In terms of using functional programming for the frontend: it became difficult when needing to update multiple elements based on certain individual conditions. For example, we might have needed to abstract a function to update multiple elements in an `if` block – when in other (non functional) languages, our updates could have been done line after line in the `then` blocks without the need for a separate method.
