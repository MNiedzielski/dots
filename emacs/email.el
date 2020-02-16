From southern@isis.cgd.ucar.EDU Thu Mar 25 08:39:25 1993
From: southern@isis.cgd.ucar.EDU (Lawrence Buja)
Subject: Re: email
To: british-cars@transfer.stratus.com (british-cars)
Date: Thu, 25 Mar 93 6:27:18 MST
X-Mailer: ELM [version 2.3 PL11]
Content-Length: 2790
Status: RO
X-Lines: 66

T Cox asks....
}What is the consensus of the best email system?  I tried to use
}microsoft email for a while and though it has its merits, it is not
}very good for long files such as the brit car mail list.

Asking questions like this and not telling us what kind of system you're
running on is like asking specific engine questions and not saying what
kind it is.  I assume that microsoft email just runs on PC's.  But since
others who use the digest may be able to use following (or suggest a
better way to do it), I'll relate how I deal with the digest.

What I use is elm on a Sun Unix box.  I get the digest to keep from
being flooded with e-mail during the day (filters are also available to
forward brit-cars e-mail to another file so it doesn't overwhelm that
other, unimportant stuff which is work related, but then the temptation
is to spend too much work time reading brit-cars e-mail and waiting for
the next one to come in).

When I logon in the morning from home, a convoluted emacs process (see
below) gets automatically run to bust the digest out into individual
mail messages.  That way I can read, delete, archive or reply to
individual messages (like this one) rather than trying to deal with one
BIG digest file.  I'm sure there's a better way to do this with sed, but
when I asked around, there didn't seem to be anything out there to do
this.  Since I didn't know sed and I do know emacs, I built this and it
works for me.

/\      Lawrence Buja           Climate and Global Dynamics Division
  \_][  southern@ncar.ucar.edu  National Center for Atmospheric Research
      \_________________________Boulder,_Colorado___80307-3000__________

The Unix command to start the dedigestifier is:

 alias dedigest '/usr/local/bin/emacs -batch $HOME/Mail/mbox.common -l \
                "/u3/southern/.emacs" -f dedigest'

$HOME/Mail/mbox.common is my "mail box" file where incoming mail
arrives.  /u3/southern/.emacs contains my homebrewed emacs functions,
including dedigest:

 (defun dedigest () (interactive) (save-buffer) (load "dedigest.el")
                 (set-mark-command nil) 
                 (dedigestify (region-beginning) (region-end))
                 (save-buffers-kill-emacs t)
 )

and it loads in the file dedigest.el which looks like:

   (defun dedigestify (start end)
     "Dedigestify the Digest for elm users"
     (interactive "r")
     (save-excursion
       (set-mark-command nil)
       (mark-whole-buffer)
       (save-restriction
        (goto-char (point-min))
       (replace-regexp "----

Date:" "----
>From asdf  Mon Mar 16 12:42:39 1992
Date:" 
       ))))

mbox.common will the contain the dedigested digest as well as any other
mail in the file.  A backup file called mbox.common.~1~ will also be
generated containing the original digest.

