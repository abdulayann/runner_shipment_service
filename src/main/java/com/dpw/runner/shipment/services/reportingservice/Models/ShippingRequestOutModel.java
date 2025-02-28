package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentAndContainerResponse;
import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShippingRequestOutModel implements IDocumentModel {
    private ShipmentModel shipment;
    private ConsolidationModel consolidation;
    private UnlocationsResponse loadingPort;
    private UnlocationsResponse dischargePort;
    private TenantModel tenant;
    private UsersDto user;
    private List<ShipmentContainers> consolContainers;
    private VesselsResponse vessel;
    private CarrierMasterData carrier;
    private String serviceMode;
    @JsonProperty("ShipmentAndContainer")
    private List<ShipmentAndContainerResponse> ShipmentAndContainer;
    private List<ShipmentModel> shipmentList;
    private List<ContainerModel> commonContainers;
}
