package com.dpw.runner.shipment.services.kafka.dto.inttra;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
public class HaulagePartyDto implements Serializable {
    private HaulageParty haulageParty;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime containerCutOff;
}
