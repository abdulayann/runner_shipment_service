package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_RECEIVING_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_RECEIVING_AGENT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_SENDING_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_SENDING_AGENT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_TOTAL_PACKS_AND_UNITS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_AIRCRAFT_TYPE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_FIRST_FLIGHT_AND_DAY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_FLIGHT_CARRIER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CURRENT_DATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.IS_CONSOL_SECURITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORG_DATA_NOT_AVAILABLE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RECEIVING_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RECEIVING_AGENT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SCI;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SENDING_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SENDING_AGENT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_GROSS_WEIGHT_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_PACKS_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.CarrierDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CargoManifestAirConsolidationReport extends IReport{

    @Autowired
    private IAwbDao awbDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IPackingService packingService;

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
        ShipmentSettingsDetails shipmentSettingsDetails = getCurrentShipmentSettings();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            validateAirDGAndAirSecurityCheckConsolidations(cargoManifestAirConsolidationModel.getConsolidationModel());
        } else {
            validateAirDGCheckConsolidations(cargoManifestAirConsolidationModel.getConsolidationModel());
        }
        cargoManifestAirConsolidationModel.setTenantModel(getTenant());
        cargoManifestAirConsolidationModel.setShipmentModelList(new ArrayList<>());
        cargoManifestAirConsolidationModel.setAwbList(new ArrayList<>());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(packingService.calculatePackSummary(commonUtils.convertToList(cargoManifestAirConsolidationModel.getConsolidationModel().getPackingList(), Packing.class),
                cargoManifestAirConsolidationModel.getConsolidationModel().getTransportMode(),
                cargoManifestAirConsolidationModel.getConsolidationModel().getContainerCategory(), new ShipmentMeasurementDetailsDto()));
        if(shipIds != null && !shipIds.isEmpty()) {
            Map<Long, ShipmentModel> shipmentModelMap = getShipments(shipIds);
            List<Awb> awbListPage = awbDao.findByShipmentIdList(shipIds);
            Map<Long, List<Awb>> awbMap = new HashMap<>();
            if(awbListPage != null && !awbListPage.isEmpty()) {
                awbMap = awbListPage.stream().collect(Collectors.groupingBy(Awb::getShipmentId));
            }
            for(Map.Entry<Long, ShipmentModel> entry: shipmentModelMap.entrySet()) {
                cargoManifestAirConsolidationModel.getShipmentModelList().add(shipmentModelMap.get(entry.getKey()));
                if(awbMap.containsKey(entry.getKey()))
                    cargoManifestAirConsolidationModel.getAwbList().add(awbMap.get(entry.getKey()).get(0));
                else
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
        dictionary.put(IS_CONSOL_SECURITY, isSecurityData);
        dictionary = populateHAWBAndSecurityData(cargoManifestAirConsolidationModel.getShipmentModelList(), cargoManifestAirConsolidationModel.getAwbList(), dictionary, isSecurityData, isShipperAndConsignee, true);
        populateRaKcDataConsolidation(dictionary, cargoManifestAirConsolidationModel.getConsolidationModel());
        boolean airRoutingTagsAdded = getAirRoutingFlightTags(cargoManifestAirConsolidationModel.getConsolidationModel().getRoutingsList(), dictionary, false);
        CarrierDetailModel carrierDetailModel = cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails();
        if(!airRoutingTagsAdded) {
            Map<String, CarrierMasterData> carriersMap = new HashMap<>();
            if(!CommonUtils.isStringNullOrEmpty(carrierDetailModel.getShippingLine()))
                carriersMap = masterDataUtils.getCarriersData(Set.of(carrierDetailModel.getShippingLine()));
            dictionary.put(CONSOL_FIRST_FLIGHT_AND_DAY, getFlightAndDayString(carriersMap, carrierDetailModel.getShippingLine(), carrierDetailModel.getFlightNumber(), carrierDetailModel.getEtd()));
        }
        dictionary.put(TOTAL_GROSS_WEIGHT_UNIT, cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacksWeight());
        dictionary.put(TOTAL_PACKS_UNIT, (Constants.MPK).equals(cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacks()) ? Constants.PACKAGES : cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacks());
        dictionary.put(CM_TOTAL_PACKS_AND_UNITS, getTotalPacksAndUnit(cargoManifestAirConsolidationModel.getPackSummaryResponse()));
        dictionary.put(CONSOL_FLIGHT_CARRIER, cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails().getShippingLine());
        dictionary.put(CONSOL_AIRCRAFT_TYPE, cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails().getAircraftType());
        dictionary.put(CURRENT_DATE, convertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirConsolidationModel.getTenantModel());
        try{ dictionary.put(SENDING_AGENT_NAME, cargoManifestAirConsolidationModel.getConsolidationModel().getSendingAgent().getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(SENDING_AGENT_ADDRESS, getOrgAddress(cargoManifestAirConsolidationModel.getConsolidationModel().getSendingAgent())); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(RECEIVING_AGENT_NAME, cargoManifestAirConsolidationModel.getConsolidationModel().getReceivingAgent().getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(RECEIVING_AGENT_ADDRESS, getOrgAddress(cargoManifestAirConsolidationModel.getConsolidationModel().getReceivingAgent())); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        dictionary.put(SCI, cargoManifestAirConsolidationModel.getConsolidationModel().getSci());
        populateConsolidationCargoManifestParty(cargoManifestAirConsolidationModel.getConsolidationModel(), dictionary);
        this.populateConsolidationReportData(dictionary, null, cargoManifestAirConsolidationModel.getConsolidationModel().getId());
        return dictionary;
    }

    private void populateConsolidationCargoManifestParty(ConsolidationModel consolidationModel, Map<String, Object> dictionary) {
        // Sending Agent
        var shipmentOriginAgent = consolidationModel.getSendingAgent();
        dictionary.put(CM_SENDING_AGENT_NAME, dictionary.get(SENDING_AGENT_NAME));
        ReportHelper.populateCargoManifestPartyAddress(dictionary, shipmentOriginAgent, CM_SENDING_AGENT_ADDRESS);

        // Receiving Agent
        var shipmentDestinationAgent = consolidationModel.getReceivingAgent();
        dictionary.put(CM_RECEIVING_AGENT_NAME, dictionary.get(RECEIVING_AGENT_NAME));
        ReportHelper.populateCargoManifestPartyAddress(dictionary, shipmentDestinationAgent, CM_RECEIVING_AGENT_ADDRESS);
    }

    public String getTotalPacksAndUnit(PackSummaryResponse packSummaryResponse) {
        String totalPacksAndUnit = packSummaryResponse.getTotalPacks();
        try {
            String[] packsAndUnit = totalPacksAndUnit.split(",");
            int totalQuantity = 0;
            for (String pack : packsAndUnit) {
                String[] quantityAndType = pack.strip().split(" ");
                totalQuantity += Integer.parseInt(quantityAndType[0]);
            }
            totalPacksAndUnit = totalQuantity + " " + Constants.PIECES;
        }
        catch (Exception ignored) {
            // ignored
        }

        return (Constants.MPK.equals(totalPacksAndUnit) ? Constants.PACKAGES : totalPacksAndUnit);
    }


    public void setShipIds(List<Long> shipIds) {
        this.shipIds = shipIds;
    }

    public void setShipperAndConsignee(boolean shipperAndConsignee) {
        isShipperAndConsignee = shipperAndConsignee;
    }

    public void setSecurityData(boolean securityData) {
        isSecurityData = securityData;
    }
}
