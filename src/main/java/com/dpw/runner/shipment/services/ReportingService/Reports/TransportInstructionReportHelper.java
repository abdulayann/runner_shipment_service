package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsContainersModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsPackagesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsReferenceModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsTruckDriverModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getFormattedAddress;

@Component
public class TransportInstructionReportHelper {

    @Autowired
    private ModelMapper modelMapper;


    public void addTransportInstructionLegsDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        legsDictionary.put(ReportConstants.HAS_LEGS, true);
        TILegsModel tiLegsModel = modelMapper.map(tilegs, TILegsModel.class);
        tiLegsModel.setLegType(tilegs.getLegType().getDescription());
        tiLegsModel.setOrigin(getOriginFullName(tilegs));
        if (tilegs.getOrigin() != null) {
            tiLegsModel.setOriginAddress(getFormattedAddress(modelMapper.map(tilegs.getOrigin(), PartiesModel.class), false));
        }
        tiLegsModel.setDestination(tilegs.getDestination() != null ? tilegs.getDestination().getOrgData().get(FULL_NAME) : Constants.EMPTY_STRING);
        if (tilegs.getDestination() != null) {
            tiLegsModel.setDestinationAddress(getFormattedAddress(modelMapper.map(tilegs.getDestination(), PartiesModel.class), false));
        }
        legsDictionary.put(ReportConstants.TI_LEGS, tiLegsModel);
    }

    private static Object getOriginFullName(TiLegs tilegs) {
        if (tilegs.getOrigin() != null) {
            Map<String, Object> orgData = tilegs.getOrigin().getOrgData();
            if (orgData != null) {
              return orgData.get(FULL_NAME);
            }
        }
        return Constants.EMPTY_STRING;
    }

    public void addTransportInstructionLegsTruckDriverDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        List<TiTruckDriverDetails> tiTruckDriverDetails = tilegs.getTiTruckDriverDetails();
        if (!CommonUtils.listIsNullOrEmpty(tiTruckDriverDetails)) {
            legsDictionary.put(ReportConstants.HAS_TRUCK_DRIVERS, true);
            List<TILegsTruckDriverModel> tiLegsTruckDriverModels = tiTruckDriverDetails.stream()
                    .map(truckDriverDetails -> modelMapper.map(truckDriverDetails, TILegsTruckDriverModel.class))
                    .toList();
            legsDictionary.put(ReportConstants.TI_TRUCK_DRIVERS, tiLegsTruckDriverModels);
        }
    }

    public void addTransportInstructionLegsReferencesDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        List<TiReferences> tiReferences = tilegs.getTiReferences();
        if (!CommonUtils.listIsNullOrEmpty(tiReferences)) {
            legsDictionary.put(ReportConstants.HAS_REFERENCE_DETAILS, true);
            List<TILegsReferenceModel> tiLegsReferenceModels = tiReferences.stream()
                    .map(references -> modelMapper.map(references, TILegsReferenceModel.class))
                    .toList();
            legsDictionary.put(ReportConstants.TI_REFERENCES, tiLegsReferenceModels);
        }
    }

    public void addTransportInstructionLegsPackagesDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        List<TiPackages> tiPackages = tilegs.getTiPackages();
        if (!CommonUtils.listIsNullOrEmpty(tiPackages)) {
            legsDictionary.put(ReportConstants.HAS_PACKAGE_DETAILS, true);
            List<TILegsPackagesModel> tiLegsPackagesModels = tiPackages.stream()
                    .map(packages -> modelMapper.map(packages, TILegsPackagesModel.class))
                    .toList();
            legsDictionary.put(ReportConstants.TI_PACKAGES, tiLegsPackagesModels);
        }
    }

    public void addTransportInstructionLegsContainersDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        List<TiContainers> tiContainers = tilegs.getTiContainers();
        if (!CommonUtils.listIsNullOrEmpty(tiContainers)) {
            legsDictionary.put(ReportConstants.HAS_CONTAINERS, true);
            List<TILegsContainersModel> tiLegsContainersModels = tiContainers.stream()
                    .map(containers -> modelMapper.map(containers, TILegsContainersModel.class))
                    .toList();

            legsDictionary.put(ReportConstants.TI_CONTAINERS, tiLegsContainersModels);

        }
    }
}
