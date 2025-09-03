package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class HaulagePartyDto implements Serializable {
   private HaulageParty haulageParty;
   private LocalDateTime containerCutOff;
}
