module CommandMap (commandMap) where

import Commands.Commands
import Commands.Types ( Command )
import Data.Map ( Map, fromList )
import Data.Text ( Text )

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
      ("os", osCom),
      ("calcstat", csCom),
      ("cs", csCom),
      ("ms", msCom),
      ("maxstat", msCom),
      ("bu", beatUpCom),
      ("beatup", beatUpCom),
      ("ct", ctCom),
      ("calculatetries", ctCom),
      ("cc", ccCom),
      ("calcchance", ccCom),
      ("ldn", dnCom)
    ]
