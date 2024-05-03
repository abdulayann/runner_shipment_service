package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
@Data
public class CargoManifestAirConsolidationReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingService packingService;

    private List<Awb> awbList;

    private List<Long> shipIds;

    private boolean isShipperAndConsignee;

    private boolean isSecurityData;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = (CargoManifestAirConsolidationModel) getDocumentModel(id);
        return populateDictionary(cargoManifestAirConsolidationModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = new CargoManifestAirConsolidationModel();
        cargoManifestAirConsolidationModel.setConsolidationModel(getConsolidation(id));
        cargoManifestAirConsolidationModel.setTenantModel(getTenant());
        cargoManifestAirConsolidationModel.setShipmentModelList(new ArrayList<>());
        cargoManifestAirConsolidationModel.setAwbList(new ArrayList<>());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(packingService.calculatePackSummary(jsonHelper.convertValueToList(cargoManifestAirConsolidationModel.getConsolidationModel().getPackingList(), Packing.class),
                cargoManifestAirConsolidationModel.getConsolidationModel().getTransportMode(),
                cargoManifestAirConsolidationModel.getConsolidationModel().getContainerCategory(), new ShipmentMeasurementDetailsDto()));
        if(awbList != null && !awbList.isEmpty()) {
            Map<Long, ShipmentModel> shipmentModelMap = getShipments(shipIds);
            for(Awb awb: awbList) {
                if(shipmentModelMap.containsKey(awb.getShipmentId())) {
                    cargoManifestAirConsolidationModel.getShipmentModelList().add(shipmentModelMap.get(awb.getShipmentId()));
                    cargoManifestAirConsolidationModel.getAwbList().add(awb);
                }
            }
        }
        else if(shipIds != null && !shipIds.isEmpty()) {
            Map<Long, ShipmentModel> shipmentModelMap = getShipments(shipIds);
            for(Map.Entry<Long, ShipmentModel> entry: shipmentModelMap.entrySet()) {
                cargoManifestAirConsolidationModel.getShipmentModelList().add(shipmentModelMap.get(entry.getKey()));
                cargoManifestAirConsolidationModel.getAwbList().add(null);
            }
        }
        return cargoManifestAirConsolidationModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = (CargoManifestAirConsolidationModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateConsolidationFields(cargoManifestAirConsolidationModel.getConsolidationModel(), dictionary);
        dictionary = populateHAWBAndSecurityData(cargoManifestAirConsolidationModel.getShipmentModelList(), cargoManifestAirConsolidationModel.getAwbList(), dictionary, isSecurityData, isShipperAndConsignee, true);
        dictionary.put(TOTAL_GROSS_WEIGHT_UNIT, cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacksWeight());
        dictionary.put(TOTAL_PACKS_UNIT, cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacks());
        dictionary.put(CONSOL_FLIGHT_CARRIER, cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails().getShippingLine());
        dictionary.put(CONSOL_AIRCRAFT_TYPE, cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails().getAircraftType());
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirConsolidationModel.getTenantModel());
        return dictionary;
    }
}
