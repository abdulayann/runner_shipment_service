package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CargoManifestAirShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
public class CargoManifestAirShipmentReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAwbDao awbDao;

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = (CargoManifestAirShipmentModel) getDocumentModel(id);
        return populateDictionary(cargoManifestAirShipmentModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CargoManifestAirShipmentModel cargoManifestAirShipmentModel = new CargoManifestAirShipmentModel();
        cargoManifestAirShipmentModel.shipmentDetails = getShipment(id);
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
        Map<ShipmentModel, Awb> map = new HashMap<>();
        map.put(cargoManifestAirShipmentModel.shipmentDetails, cargoManifestAirShipmentModel.awb);
        populateHAWBAndSecurityData(map, dictionary);
        return dictionary;
    }
}
