package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

@Component
public class CargoManifestAirShipmentReport extends IReport{

    @Autowired
    private IAwbDao awbDao;

    public boolean isShipperAndConsignee;

    public boolean isSecurityData;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = (CargoManifestAirShipmentModel) getDocumentModel(id);
        return populateDictionary(cargoManifestAirShipmentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = new CargoManifestAirShipmentModel();
        cargoManifestAirShipmentModel.shipmentDetails = getShipment(id);
        cargoManifestAirShipmentModel.tenantModel = getTenant();
        List<Awb> awbList = awbDao.findByShipmentId(id);
        if(awbList != null && !awbList.isEmpty())
            cargoManifestAirShipmentModel.awb = awbList.get(0);
        return cargoManifestAirShipmentModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = (CargoManifestAirShipmentModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        populateShipmentFields(cargoManifestAirShipmentModel.shipmentDetails, dictionary);
        dictionary.put(ReportConstants.AIRCRAFT_TYPE, cargoManifestAirShipmentModel.shipmentDetails.getCarrierDetails().getAircraftType());
        dictionary = populateHAWBAndSecurityData(List.of(cargoManifestAirShipmentModel.shipmentDetails), List.of(cargoManifestAirShipmentModel.awb), dictionary, isSecurityData, isShipperAndConsignee, false);
        dictionary.put(CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        ReportHelper.addTenantDetails(dictionary, cargoManifestAirShipmentModel.tenantModel);
        PartiesModel originAgent = cargoManifestAirShipmentModel.shipmentDetails.getAdditionalDetails().getExportBroker();
        PartiesModel destinationAgent = cargoManifestAirShipmentModel.shipmentDetails.getAdditionalDetails().getImportBroker();
        try{ dictionary.put(ORIGIN_AGENT_NAME, originAgent.getOrgData().get(FULL_NAME)); }catch (Exception ignored) {}
        try{ dictionary.put(ORIGIN_AGENT_ADDRESS, getOrgAddress(originAgent)); }catch (Exception ignored) {}
        try{ dictionary.put(DESTINATION_AGENT_NAME, destinationAgent.getOrgData().get(FULL_NAME)); }catch (Exception ignored) {}
        try{ dictionary.put(DESTINATION_AGENT_ADDRESS, getOrgAddress(destinationAgent)); }catch (Exception ignored) {}
        dictionary.put(ReportConstants.WITH_CONSIGNOR, isShipperAndConsignee);
        return dictionary;
    }
}
