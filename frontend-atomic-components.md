# Front Row Atomic Components

We are creating a set of base components that will standardize user experience across the teacher app.

## Motivation
The main motivation for this switch is to avoid having to repeat pieces of UX or UI, and to easily maintain consistency in all pages.

Base components will be stored in `frontend-entities/components/base`.

## Atoms

Atoms will represent the smallest piece of UI/UX possible. There will be the building block of other pieces.

It will include (not fully defined yet):
 * Button
 * Link
 * Form fields (Input, Select, ...)
 * Label
 * Heading (H1, H2, ...)

Atoms should be flexible enough to be able to meet all your needs. If you need a different behavior you probably want to create another component encapsulating an atom (eg ButtonToggle)

## Molecules

Molecules represent pieces of UI containing atoms and its style.
The goal of molecules is to make a standard for larger piece of UI.

Example of molecules:
 * Modal
 * Panel
