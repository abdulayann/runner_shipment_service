package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.CarrierDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

@Component
@Data
@Slf4j
public class CargoManifestAirShipmentReport extends IReport{

    @Autowired
    private IAwbDao awbDao;

    private boolean isShipperAndConsignee;

    private boolean isSecurityData;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = (CargoManifestAirShipmentModel) getDocumentModel(id);
        return populateDictionary(cargoManifestAirShipmentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = new CargoManifestAirShipmentModel();
        cargoManifestAirShipmentModel.setShipmentDetails(getShipment(id));
        ShipmentSettingsDetails shipmentSettingsDetails = getCurrentShipmentSettings();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            validateAirDGAndAirSecurityCheckShipments(cargoManifestAirShipmentModel.getShipmentDetails());
        } else {
            validateAirDGCheckShipments(cargoManifestAirShipmentModel.getShipmentDetails());
        }
        validateAirAndOceanDGCheck(cargoManifestAirShipmentModel.getShipmentDetails()); // check if for consolidation required
        cargoManifestAirShipmentModel.setTenantModel(getTenant());
        List<Awb> awbList = awbDao.findByShipmentId(id);
        if(awbList != null && !awbList.isEmpty())
            cargoManifestAirShipmentModel.setAwb(awbList.get(0));
        return cargoManifestAirShipmentModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = (CargoManifestAirShipmentModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateShipmentFields(cargoManifestAirShipmentModel.getShipmentDetails(), dictionary);
        dictionary.put(ReportConstants.AIRCRAFT_TYPE, cargoManifestAirShipmentModel.getShipmentDetails().getCarrierDetails().getAircraftType());
        List<Awb> awbList = new ArrayList<>();
        awbList.add(cargoManifestAirShipmentModel.getAwb());
        dictionary.put(IS_SECURITY, isSecurityData);
        dictionary = populateHAWBAndSecurityData(List.of(cargoManifestAirShipmentModel.getShipmentDetails()), awbList, dictionary, isSecurityData, isShipperAndConsignee, false);
        boolean airRoutingTagsAdded = getAirRoutingFlightTags(cargoManifestAirShipmentModel.getShipmentDetails().getRoutingsList(), dictionary, true);
        CarrierDetailModel carrierDetailModel = cargoManifestAirShipmentModel.getShipmentDetails().getCarrierDetails();
        if(!airRoutingTagsAdded) {
            Map<String, CarrierMasterData> carriersMap = new HashMap<>();
            if(!CommonUtils.IsStringNullOrEmpty(carrierDetailModel.getShippingLine()))
                carriersMap = masterDataUtils.getCarriersData(Set.of(carrierDetailModel.getShippingLine()));
            dictionary.put(SHIPMENT_FIRST_FLIGHT_AND_DAY, getFlightAndDayString(carriersMap, carrierDetailModel.getShippingLine(), carrierDetailModel.getFlightNumber(), carrierDetailModel.getEtd()));
        }
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirShipmentModel.getTenantModel());
        PartiesModel originAgent = cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails().getExportBroker();
        PartiesModel destinationAgent = cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails().getImportBroker();
        try{ dictionary.put(ORIGIN_AGENT_NAME, originAgent.getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(ORIGIN_AGENT_ADDRESS, getOrgAddress(originAgent)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(DESTINATION_AGENT_NAME, destinationAgent.getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(DESTINATION_AGENT_ADDRESS, getOrgAddress(destinationAgent)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        dictionary.put(CM_ORIGIN_AGENT_NAME, dictionary.get(ORIGIN_AGENT_NAME));
        dictionary.put(CM_DESTINATION_AGENT_NAME, dictionary.get(DESTINATION_AGENT_NAME));
        dictionary.put(ReportConstants.WITH_CONSIGNOR, isShipperAndConsignee);
        dictionary.put(SCI, cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails().getSci());
        if(cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails().getNotifyParty()));
        }
        dictionary.put(CM_NO_OF_PACKAGES, cargoManifestAirShipmentModel.getShipmentDetails().getNoOfPacks());
        dictionary.put(CM_PACKS_UNIT, Constants.PIECES);
        return dictionary;
    }
}
