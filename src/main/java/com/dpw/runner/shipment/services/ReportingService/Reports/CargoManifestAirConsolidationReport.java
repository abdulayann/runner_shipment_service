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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class CargoManifestAirConsolidationReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IPackingService packingService;

    public List<Awb> awbList;

    public List<Long> shipIds;

    public boolean isShipperAndConsignee;

    public boolean isSecurityData;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = (CargoManifestAirConsolidationModel) getDocumentModel(id);
        return populateDictionary(cargoManifestAirConsolidationModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = new CargoManifestAirConsolidationModel();
        cargoManifestAirConsolidationModel.consolidationModel = getConsolidation(id);
        cargoManifestAirConsolidationModel.tenantModel = getTenant();
        cargoManifestAirConsolidationModel.shipmentModelList = new ArrayList<>();
        cargoManifestAirConsolidationModel.awbList = new ArrayList<>();
        cargoManifestAirConsolidationModel.packSummaryResponse = packingService.calculatePackSummary(jsonHelper.convertValueToList(cargoManifestAirConsolidationModel.consolidationModel.getPackingList(), Packing.class),
                cargoManifestAirConsolidationModel.consolidationModel.getTransportMode(),
                cargoManifestAirConsolidationModel.consolidationModel.getContainerCategory(), new ShipmentMeasurementDetailsDto());
        if(awbList != null && !awbList.isEmpty()) {
            Map<Long, ShipmentModel> shipmentModelMap = getShipments(shipIds);
            for(Awb awb: awbList) {
                if(shipmentModelMap.containsKey(awb.getShipmentId())) {
                    cargoManifestAirConsolidationModel.shipmentModelList.add(shipmentModelMap.get(awb.getShipmentId()));
                    cargoManifestAirConsolidationModel.awbList.add(awb);
                }
            }
        }
        else if(shipIds != null && !shipIds.isEmpty()) {
            Map<Long, ShipmentModel> shipmentModelMap = getShipments(shipIds);
            for(Long shipId: shipmentModelMap.keySet()) {
                cargoManifestAirConsolidationModel.shipmentModelList.add(shipmentModelMap.get(shipId));
                cargoManifestAirConsolidationModel.awbList.add(null);
            }
        }
        return cargoManifestAirConsolidationModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestAirConsolidationModel cargoManifestAirConsolidationModel = (CargoManifestAirConsolidationModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateConsolidationFields(cargoManifestAirConsolidationModel.consolidationModel, dictionary);
        dictionary = populateHAWBAndSecurityData(cargoManifestAirConsolidationModel.shipmentModelList, cargoManifestAirConsolidationModel.awbList, dictionary, isSecurityData, isShipperAndConsignee, true);
        dictionary.put(TOTAL_GROSS_WEIGHT_UNIT, cargoManifestAirConsolidationModel.packSummaryResponse.getTotalPacksWeight());
        dictionary.put(TOTAL_PACKS_UNIT, cargoManifestAirConsolidationModel.packSummaryResponse.getTotalPacks());
        dictionary.put(CONSOL_FLIGHT_CARRIER, cargoManifestAirConsolidationModel.consolidationModel.getCarrierDetails().getShippingLine());
        dictionary.put(CONSOL_AIRCRAFT_TYPE, cargoManifestAirConsolidationModel.consolidationModel.getCarrierDetails().getAircraftType());
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirConsolidationModel.tenantModel);
        return dictionary;
    }
}
