# Libraries ----
if (!require(pacman)) install.packages("pacman")
p_load(quanteda, nsyllable, tidyverse)

# Functions ----
flesch_score <- function(passage) {
  sentences <- nsentence(passage)
  syllables <- nsyllable(passage)
  words <- ntoken(passage, remove_punct = T)
  
  words_sentences <- - (1.015 * (words / sentences))
  syllables_words <- - (84.6 * (syllables / words))
  fres <- 206.835 - 1.015 * (words / sentences) - 84.6 * (syllables / words)
  
  return (
    tibble(
      type = c(
        "Words/sentences",
        "Syllables/words",
        "Flesch Reading Ease Score"
      ),
      value = c(
        words_sentences,
        syllables_words,
        fres
      )
    )
  )
}
flesch_grade <- function(passage) {
  sentences <- nsentence(passage)
  syllables <- nsyllable(passage)
  words <- ntoken(passage, remove_punct = T)
  
  fgl <- 0.39 * (words / sentences) + 11.8 * (syllables / words) - 15.59
  
  return (fgl)
}

# Use of functions ----
## Declare passage objects ##
longest_sent_philo <- # source: https://www.stephenhicks.org/2024/10/31/philosophys-longest-sentence-kant-over-kierkegaard/
  "How mystical enthusiasms in the life of hermits and monks and the glorification of the sanctity of the celibate state made a large number of people useless to the world; how the alleged miracles connected with this oppressed the people with heavy chains of blind superstition; how, with a hierarchy imposing itself on free people, the terrible voice of orthodoxy arose from the mouths of presumptuous, solely appointed interpreters of the Scriptures and divided the Christian world into bitter factions because of religious opinions (for which, unless one calls upon pure reason as the interpreter, there is absolutely no general agreement to be found); as in the East, where the state itself, in a ridiculous manner, concerned itself with the religious statutes of the priests and clergy, instead of keeping them within the narrow limits of a mere teaching profession (from which they are always inclined to pass into a ruling one), how, I say, this state finally inevitably had to become a prey to foreign enemies, who finally put an end to its dominant faith; as in the West, where faith has established its own throne, independent of secular power, the civil order, together with the sciences (which sustain it), were shattered and rendered powerless by an assumed representative of God; how both Christian parts of the world were attacked by barbarians, like plants and animals which, close to their dissolution by a disease, attract destructive insects to complete them; how in the latter case that spiritual leader ruled and chastised kings like children with the magic rod of his threatened ban, inciting them to foreign wars that depopulated another part of the world (the Crusades), to hostility among themselves, to rebellion of the subjects against their superiors and to bloodthirsty hatred against their fellow-members of the same general so-called Christianity who think differently; how the roots of this discord, which even now is only prevented from violent outbreaks by political interests, lie hidden in the principle of a despotically commanding church belief and still gives rise to fears of similar events:—this history of Christianity (which, insofar as it was to be based on a belief in history, could not have turned out any differently), if one takes it as a picture at a glance, could well justify the exclamation: tantum religio potuit suadere malorum! if it were not still clear enough from its foundation that its true first intention was none other than to introduce a pure religious belief about which there can be no conflicting opinions, and that all that turmoil by which the human race has been and is still being divided stems only from the fact that through a bad tendency of human nature, what was initially intended to serve to introduce the latter, namely, to win the nation accustomed to the old historical belief for the new one through its own prejudices, was subsequently made the foundation of a universal world religion."
wiki_short_valid <- "The cat sat on the mat."
wiki_long_valid <- # Marcel Proust
  "But I had seen first one and then another of the rooms in which I had slept during my life, and in the end I would revisit them all in the long course of my waking dream: rooms in winter, where on going to bed I would at once bury my head in a nest, built up out of the most diverse materials, the corner of my pillow, the top of my blankets, a piece of a shawl, the edge of my bed, and a copy of an evening paper, all of which things I would contrive, with the infinite patience of birds building their nests, to cement into one whole; rooms where, in a keen frost, I would feel the satisfaction of being shut in from the outer world (like the sea-swallow which builds at the end of a dark tunnel and is kept warm by the surrounding earth), and where, the fire keeping in all night, I would sleep wrapped up, as it were, in a great cloak of snug and savoury air, shot with the glow of the logs which would break out again in flame: in a sort of alcove without walls, a cave of warmth dug out of the heart of the room itself, a zone of heat whose boundaries were constantly shifting and altering in temperature as gusts of air ran across them to strike freshly upon my face, from the corners of the room, or from parts near the window or far from the fireplace which had therefore remained cold—or rooms in summer, where I would delight to feel myself a part of the warm evening, where the moonlight striking upon the half-opened shutters would throw down to the foot of my bed its enchanted ladder; where I would fall asleep, as it might be in the open air, like a titmouse which the breeze keeps poised in the focus of a sunbeam—or sometimes the Louis XVI room, so cheerful that I could never feel really unhappy, even on my first night in it: that room where the slender columns which lightly supported its ceiling would part, ever so gracefully, to indicate where the bed was and to keep it separate; sometimes again that little room with the high ceiling, hollowed in the form of a pyramid out of two separate storeys, and partly walled with mahogany, in which from the first moment my mind was drugged by the unfamiliar scent of flowering grasses, convinced of the hostility of the violet curtains and of the insolent indifference of a clock that chattered on at the top of its voice as though I were not there; while a strange and pitiless mirror with square feet, which stood across one corner of the room, cleared for itself a site I had not looked to find tenanted in the quiet surroundings of my normal field of vision: that room in which my mind, forcing itself for hours on end to leave its moorings, to elongate itself upwards so as to take on the exact shape of the room, and to reach to the summit of that monstrous funnel, had passed so many anxious nights while my body lay stretched out in bed, my eyes staring upwards, my ears straining, my nostrils sniffing uneasily, and my heart beating; until custom had changed the colour of the curtains, made the clock keep quiet, brought an expression of pity to the cruel, slanting face of the glass, disguised or even completely dispelled the scent of flowering grasses, and distinctly reduced the apparent loftiness of the ceiling."

## Flesch-Kincaid Reading Ease score ##
flesch_score(longest_sent_philo)
flesch_score(wiki_short_valid)
flesch_score(wiki_long_valid)

## Flesch-Kincaid Grade Level
flesch_grade(wiki_short_valid)