package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

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
        cargoManifestAirConsolidationModel.setTenantModel(getTenant());
        cargoManifestAirConsolidationModel.setShipmentModelList(new ArrayList<>());
        cargoManifestAirConsolidationModel.setAwbList(new ArrayList<>());
        cargoManifestAirConsolidationModel.setPackSummaryResponse(packingService.calculatePackSummary(commonUtils.convertToList(cargoManifestAirConsolidationModel.getConsolidationModel().getPackingList(), Packing.class),
                cargoManifestAirConsolidationModel.getConsolidationModel().getTransportMode(),
                cargoManifestAirConsolidationModel.getConsolidationModel().getContainerCategory(), new ShipmentMeasurementDetailsDto()));
        if(shipIds != null && !shipIds.isEmpty()) {
            Map<Long, ShipmentModel> shipmentModelMap = getShipments(shipIds);
            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.SHIPMENT_ID, shipIds, "IN");
            Pair<Specification<Awb>, Pageable> pair = fetchData(listCommonRequest, Awb.class);
            Page<Awb> awbListPage = awbDao.findAll(pair.getLeft(), pair.getRight());
            Map<Long, List<Awb>> awbMap = new HashMap<>();
            if(awbListPage != null && !awbListPage.isEmpty()) {
                awbMap = awbListPage.getContent().stream().collect(Collectors.groupingBy(Awb::getShipmentId));
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
        dictionary = populateHAWBAndSecurityData(cargoManifestAirConsolidationModel.getShipmentModelList(), cargoManifestAirConsolidationModel.getAwbList(), dictionary, isSecurityData, isShipperAndConsignee, true);
        dictionary.put(TOTAL_GROSS_WEIGHT_UNIT, cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacksWeight());
        dictionary.put(TOTAL_PACKS_UNIT, cargoManifestAirConsolidationModel.getPackSummaryResponse().getTotalPacks());
        dictionary.put(CONSOL_FLIGHT_CARRIER, cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails().getShippingLine());
        dictionary.put(CONSOL_AIRCRAFT_TYPE, cargoManifestAirConsolidationModel.getConsolidationModel().getCarrierDetails().getAircraftType());
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirConsolidationModel.getTenantModel());
        try{ dictionary.put(SENDING_AGENT_NAME, cargoManifestAirConsolidationModel.getConsolidationModel().getSendingAgent().getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(SENDING_AGENT_ADDRESS, getOrgAddress(cargoManifestAirConsolidationModel.getConsolidationModel().getSendingAgent())); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(RECEIVING_AGENT_NAME, cargoManifestAirConsolidationModel.getConsolidationModel().getReceivingAgent().getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(RECEIVING_AGENT_ADDRESS, getOrgAddress(cargoManifestAirConsolidationModel.getConsolidationModel().getReceivingAgent())); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        return dictionary;
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
