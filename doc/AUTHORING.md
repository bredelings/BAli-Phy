# Authoring documentation

See [README.md](README.md) for source locations and [BUILDING.md](BUILDING.md) for builds
and validation. The conventions below describe the DocBook user guide. The tutorial and
Instructions use DocBook with an additional command-transcript preprocessing step.

## Mathematics

The source is ordinary XML in `UserGuide.xml`. Use `\(…\)` for inline mathematics and
`\[…\]` for display mathematics. Keep each expression in one text segment, without
XML markup inside it. XML still requires escaping `<` and `&` inside TeX.
Dollar signs are ordinary text, including in shell examples.

## Notes, cautions, and warnings

Keep information needed for the current explanation or next step in the main text. Use
`<note>` for self-contained supplementary material, and `<caution>` or `<warning>` for
consequences readers should see before acting. Warnings identify actions that could discard
information or stop work unexpectedly. Use `<important>` for an essential prerequisite,
such as removing quarantine before extracting the macOS archive. Give each box a descriptive `<title>` that includes
its type, such as “Caution: Preserve branch annotations”. Move relevant prose into the box
rather than repeating it. Informational boxes use `<note>`; `<info>` contains metadata.

## Commands, files, and expressions

Use `<screen>` for terminal commands and transcripts, with `<prompt>`, `<userinput>`, and
`<computeroutput>` identifying their parts. Keep short command/output exchanges together.

Use `<programlisting role="expression" language="bali-phy-model">` for model expressions.
A multiline expression listing may compare alternatives or show equivalent expressions;
it does not necessarily represent a file or script. Explain the relationship in the preceding
prose. `language` identifies syntax, while `role` identifies the purpose of the example.
Use `<synopsis role="expression">` for schematic syntax without model-language highlighting.

Give file contents and excerpts a caption identifying their filename or format. Mark excerpts
as such, and do not invent filenames for generic examples:

```xml
<example role="file-content">
  <title><filename>model.config</filename></title>
  <programlisting>:smodel HKY85</programlisting>
</example>
```

Place this block between paragraphs, not inside one. A file listing may also have a `language`
attribute when highlighting is supported; it retains the file presentation. HTML uses gray
terminal blocks, white captioned files, and pale blue-gray expressions. Print retains captions
and borders. Inline filenames use plain monospace without a background.
Keep captions with the beginning of files, but allow long listings to cross page boundaries.

## Table layout

Set table column proportions with DocBook `colspec` entries in `UserGuide.xml`; the same
proportions serve HTML and PDF. Give numeric columns less room than descriptions, but
allow space for expressions in parameter-count columns. Use ordinary spaces between
model-expression components so they can wrap without splitting identifiers.

Tables may continue across pages with repeated headings; rows taller than a page must still be allowed to split. After changing tables, inspect the PDF
for overflow, split identifiers, awkward gaps, and readable continuation headings. Check narrow
HTML layouts too, where wide tables scroll. Exact pagination is not a requirement.

## Shell alternatives

Keep shared commands and explanations outside panels. Where commands or execution instructions
differ, use xslTNG's built-in panelsets:

```xml
<variablelist role="panelset">
  <varlistentry><term><phrase role="shell-label">Unix shell
    <phrase role="shell-detail">including Git Bash and MSYS2 Bash</phrase></phrase> / Command Prompt</term><listitem>
    <screen><userinput>bpy-subsample 10 &lt; C1.trees &gt; C1.10.trees</userinput></screen>
  </listitem></varlistentry>
  <varlistentry><term>PowerShell</term><listitem>
    <screen><userinput>cmd /c "bpy-subsample 10 &lt; C1.trees &gt; C1.10.trees"</userinput></screen>
  </listitem></varlistentry>
</variablelist>
```

Use consistent shell labels and combine alternatives when the complete instructions are identical.
“Unix shell” includes Linux, macOS, WSL, Git Bash, and MSYS2 Bash. Keep the Windows Bash
clarification inside each Unix-shell label, as above; the stylesheet displays it in smaller
text below “Unix shell”. Windows-only selectors use “Git Bash / MSYS2 Bash”. Keep archive
choices and other operating-system requirements in the installation sections.

HTML displays one alternative at a time; the PDF prints every label and its
content in source order. Keep version-specific explanations beside the affected commands rather
than repeating them in the common workflow.

## Navigation and pagination

Use section headings and stable IDs for navigation and cross-references. Contents lists and
PDF page references are generated from that structure. Do not duplicate contents entries or
insert manual page breaks to tune individual pages.

Keep labels and captions with the start of their content, but allow long examples and groups
of alternatives to cross pages. After substantive edits, inspect narrow HTML layouts and PDF
output for clipping, awkward wrapping, and missing characters. Exact pagination is not a requirement.

## Function reference

Edit the JSON files under `bindings/` to update descriptions, arguments, examples, and related
entries. `generate-reference.py` produces the reference from these files; edit the source
bindings rather than generated Markdown or website pages.
