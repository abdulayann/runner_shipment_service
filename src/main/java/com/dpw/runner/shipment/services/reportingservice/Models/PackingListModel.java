package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class PackingListModel extends ShipmentPrintModel implements  IDocumentModel {
    public ConsolidationModel consolidation;
    public HblModel hbl;
    public LocalDateTime shippedOnBoard;
    public UsersDto user;
}
