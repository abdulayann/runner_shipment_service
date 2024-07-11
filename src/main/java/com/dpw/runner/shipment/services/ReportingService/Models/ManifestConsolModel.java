package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class ManifestConsolModel extends BaseDocumentModel implements IDocumentModel {
    private ConsolidationModel consolidation;
    private int containerCount;
    private int shipmentCount;
    private CarrierMasterData carrierMasterData;
    //TODO    private VesselRow ?? clarifcation required
    private List<ShipmentModel> shipmentDetailsList;
    private List<ShipmentContainers> containersList;
}
