package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.entity.Hbl;
import lombok.Data;

@Data
public class BaseDocumentModel implements IDocumentModel{
    private Hbl hbl;
    private HblPartyDto hblNotifyParty;
}
