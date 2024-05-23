package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        validateAirDGCheck(cargoManifestAirShipmentModel.getShipmentDetails()); // check if for consolidation required
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
        dictionary = populateHAWBAndSecurityData(List.of(cargoManifestAirShipmentModel.getShipmentDetails()), awbList, dictionary, isSecurityData, isShipperAndConsignee, false);
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirShipmentModel.getTenantModel());
        PartiesModel originAgent = cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails().getExportBroker();
        PartiesModel destinationAgent = cargoManifestAirShipmentModel.getShipmentDetails().getAdditionalDetails().getImportBroker();
        try{ dictionary.put(ORIGIN_AGENT_NAME, originAgent.getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(ORIGIN_AGENT_ADDRESS, getOrgAddress(originAgent)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(DESTINATION_AGENT_NAME, destinationAgent.getOrgData().get(FULL_NAME)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        try{ dictionary.put(DESTINATION_AGENT_ADDRESS, getOrgAddress(destinationAgent)); }catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        dictionary.put(ReportConstants.WITH_CONSIGNOR, isShipperAndConsignee);
        return dictionary;
    }
}
