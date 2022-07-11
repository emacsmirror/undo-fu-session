# SPDX-License-Identifier: GPL-2.0-or-later

test:
	emacs -batch -l tests/undo-fu-session-test.el -f undo-fu-session-test-run-all
