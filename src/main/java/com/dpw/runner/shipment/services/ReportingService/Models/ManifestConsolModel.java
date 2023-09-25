package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class ManifestConsolModel extends BaseDocumentModel implements IDocumentModel {
    private ConsolidationDetails consolidation;
    private int containerCount;
    private int shipmentCount;
    private CarrierMasterData carrierMasterData;
    //TODO    private VesselRow ?? clarifcation required
    private List<ShipmentDetails> shipmentDetailsList;
    private List<ShipmentContainers> containersList;
}
