project = 'rules_haskell'

copyright = '2018, The rules_haskell authors'

source_suffix = '.rst'

extensions = [
    'sphinx.ext.graphviz',
    'sphinx.ext.todo',
]

master_doc = 'index'

language = None

exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

pygments_style = 'sphinx'

html_theme = 'alabaster'

html_theme_options = {
    'show_powered_by': False,
    'github_user': 'tweag',
    'github_repo': 'rules_haskell',
    'github_banner': True,
    'github_type': "star",
    'show_related': False,
    'note_bg': '#FFF59C',
}

html_show_sphinx = False

todo_include_todos = True

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, documentclass).
latex_documents = [
    (master_doc, 'rules_haskell.tex', 'rules\\_haskell Documentation',
     'Tweag I/O', 'manual'),
]
