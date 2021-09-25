module CommandMap (commandMap) where

import Commands.Commands
import Commands.Types ( Command )
import Data.Map ( Map, fromList )
import Data.Text ( Text )
import Commands.TextCommands (osCom)

commandMap :: Map Text Command
commandMap =
  fromList
    [ ("help", helpCommand),
      ("dt", dtCommand),
      ("hostauction", registerAuctionCommand),
      ("rp", registerParticipantCommand),
      ("nom", nominationCommand),
      ("b", bidCommand),
      ("endbid", endBidCommand),
      ("info", infoCommand),
      ("infouser", infoUserCommand),
      ("endauction", endAuctionCommand),
      ("undo", undoCommand),
      ("ss", speedSheetCommand),
      ("os", osCom)
    ]
