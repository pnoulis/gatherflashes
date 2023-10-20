<a name='readme-top'></a>

## About

**Gatherflashes** fetches flash-cards found within org-mode files scattered

across the filesystem.

It is a component of my very basic flash-card learning system. This system is

made possible through the **emacs** editor and specifically **org mode**.

Org-mode is an emacs package that introduces a markup language similar to

Markdown. Files written in org-mode use the **.org** extension.

A flash card is just an org-mode entry:

```text

* What is a flash card?
:PROPERTIES:
:CATEGORY: flash
:REVISION_DATE: today
:END:

*** Answer
A flash card is a concept used widely in modern systems of learning that employ
a model of repeated practice at frequent time intervals.

Typically the learner uses a page out of a notepad to write down a question.
The other side of the notepad paper is used for holding the best possible answer.

When it is time to exercise the learner reads the question, answers it
and compares his answer with the answer at the back of the flash card.
The learner grades himself based on that comparison. If one feels satisfied that
his answer matches the flash card a high grade is given, otherwise a lower.

If a high score is achieved the learner may choose to extend the time required to
re-visit the card, if not the learner continues to exercise in frequent time intervals.

```

## Getting started
### Prerequisites

- `emacs`

  version: 30.0.50

- `org-mode`

  version: 9.6.7

### Installation

```sh

# Clone package
git clone git@github.com:pnoulis/gatherflashes.git

# Build
make build

# Append the package to the emacs load path
# Check: (elisp)Library Search
echo '(push "/path/to/gatherflahses" load-path)' >> .emacs.d/init.el

```
## Usage
## Contributing

1. Fork the project
2. Open up a pull request

## License

Distributed under the GPLv3 License. See `LICENSE` for more information.

## Contact

Pavlos noulis - pavlos.noulis@gmail.com

project link - [gatherflashes](https://github.com/pnoulis/gatherflashes)
