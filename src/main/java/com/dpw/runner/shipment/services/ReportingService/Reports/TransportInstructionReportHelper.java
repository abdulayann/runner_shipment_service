package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsContainersModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsPackagesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsReferenceModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TILegsTruckDriverModel;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.entity.TiReferences;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_KEY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EMAIL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getFormattedAddress;
import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.convertToVolumeNumberFormat;
import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.convertToWeightNumberFormat;

@Slf4j
@Component
public class TransportInstructionReportHelper {

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CommonUtils commonUtils;


    public void addTransportInstructionLegsDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        legsDictionary.put(ReportConstants.HAS_LEGS, true);
        TILegsModel tiLegsModel = modelMapper.map(tilegs, TILegsModel.class);
        tiLegsModel.setLegType(tilegs.getLegType().getDescription());
        tiLegsModel.setOrigin(getOriginFullName(tilegs));
        if (tilegs.getOrigin() != null) {
            tiLegsModel.setOriginAddress(getFormattedAddress(modelMapper.map(tilegs.getOrigin(), PartiesModel.class), false));
        }
        tiLegsModel.setDestination(getDestinationFullName(tilegs));
        if (tilegs.getDestination() != null) {
            tiLegsModel.setDestinationAddress(getFormattedAddress(modelMapper.map(tilegs.getDestination(), PartiesModel.class), false));
        }
        legsDictionary.put(ReportConstants.TI_LEGS, tiLegsModel);
        setOriginContactDetails(legsDictionary, tilegs);
        setDestinationContactDetails(legsDictionary, tilegs);
        setEstimatedPickupAndDelivery(legsDictionary, tilegs);
    }

    private void setEstimatedPickupAndDelivery(Map<String, Object> legsDictionary, TiLegs tilegs) {
        if (!Objects.isNull(tilegs.getEstimatedPickup())) {
            legsDictionary.put(ReportConstants.TI_ESTIMATED_PICKUP, tilegs.getEstimatedPickup());
        }
        if (!Objects.isNull(tilegs.getEstimatedDelivery())) {
            legsDictionary.put(ReportConstants.TI_ESTIMATED_DELIVERY, tilegs.getEstimatedDelivery());
        }
    }

    private void setDestinationContactDetails(Map<String, Object> legsDictionary, TiLegs tilegs) {
        Parties destination = tilegs.getDestination();
        if (destination != null) {
            Map<String, Object> orgData = destination.getOrgData();
            if (orgData != null) {
                legsDictionary.put(ReportConstants.TI_DESINATION_EMAIL, orgData.get(EMAIL));
                legsDictionary.put(ReportConstants.TI_DESTINATION, orgData.get(FULL_NAME));
            }
            Map<String, Object> addressData = destination.getAddressData();
            if (addressData != null) {
                legsDictionary.put(ReportConstants.TI_DESTINATION_CONTACT, addressData.get(CONTACT_KEY));
                String address = commonUtils.getAddress(addressData);
                legsDictionary.put(ReportConstants.TI_DESTINATION_ADDRESS, address);
            }
        }
    }

    private void setOriginContactDetails(Map<String, Object> legsDictionary, TiLegs tilegs) {
        Parties origin = tilegs.getOrigin();
        if (origin != null) {
            Map<String, Object> orgData = origin.getOrgData();
            if (orgData != null) {
                legsDictionary.put(ReportConstants.TI_ORIGIN_EMAIL, orgData.get(EMAIL));
                legsDictionary.put(ReportConstants.TI_ORIGIN, orgData.get(FULL_NAME));
            }
            Map<String, Object> addressData = origin.getAddressData();
            if (addressData != null) {
                legsDictionary.put(ReportConstants.TI_ORIGIN_CONTACT, addressData.get(CONTACT_KEY));
                String address = commonUtils.getAddress(addressData);
                legsDictionary.put(ReportConstants.TI_ORIGIN_ADDRESS, address);
            }
        }
    }

    private static Object getDestinationFullName(TiLegs tilegs) {
        if (tilegs.getDestination() != null) {
            Map<String, Object> orgData = tilegs.getDestination().getOrgData();
            if (orgData != null) {
                return orgData.get(FULL_NAME);
            }
        }
        return Constants.EMPTY_STRING;
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
            TiTruckDriverDetails truckDriverDetails = tiTruckDriverDetails.get(0);
            setTruckDriverDetails(legsDictionary, truckDriverDetails);
        }
    }

    private void setTruckDriverDetails(Map<String, Object> legsDictionary, TiTruckDriverDetails truckDriverDetails) {
        legsDictionary.put(ReportConstants.TI_TRUCK_NUMBER_PLATE, truckDriverDetails.getTruckNumberPlate());
        legsDictionary.put(ReportConstants.TI_TRAILER_NUMBER_PLATE, truckDriverDetails.getTrailerNumberPlate());
        legsDictionary.put(ReportConstants.TI_DRIVER_NAME, truckDriverDetails.getDriverName());
        legsDictionary.put(ReportConstants.TI_DRIVER_MOBILE_NUMBER, truckDriverDetails.getDriverMobileNumber());
        legsDictionary.put(ReportConstants.TI_DRIVER_ID, truckDriverDetails.getDriverId());

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
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if (!CommonUtils.listIsNullOrEmpty(tiPackages)) {
            legsDictionary.put(ReportConstants.HAS_PACKAGE_DETAILS, true);
            List<TILegsPackagesModel> tiLegsPackagesModels = tiPackages.stream()
                    .map(packages -> modelMapper.map(packages, TILegsPackagesModel.class))
                    .toList();
            tiLegsPackagesModels.forEach(tiLegsPackagesModel -> {
                tiLegsPackagesModel.setGrossWeight(convertToWeightNumberFormat(tiLegsPackagesModel.getGrossWeight(), v1TenantSettingsResponse));
                tiLegsPackagesModel.setNetWeight(convertToWeightNumberFormat(tiLegsPackagesModel.getNetWeight(), v1TenantSettingsResponse));
                tiLegsPackagesModel.setVolume(convertToVolumeNumberFormat(tiLegsPackagesModel.getVolume(), v1TenantSettingsResponse));
            });
            legsDictionary.put(ReportConstants.TI_PACKAGES, tiLegsPackagesModels);
        }
    }

    public void addTransportInstructionLegsContainersDataIntoDictionary(TiLegs tilegs, Map<String, Object> legsDictionary) {
        List<TiContainers> tiContainers = tilegs.getTiContainers();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if (!CommonUtils.listIsNullOrEmpty(tiContainers)) {
            legsDictionary.put(ReportConstants.HAS_CONTAINERS, true);
            List<TILegsContainersModel> tiLegsContainersModels = tiContainers.stream()
                    .map(containers -> modelMapper.map(containers, TILegsContainersModel.class))
                    .toList();
            tiLegsContainersModels.forEach(tiLegsContainersModel -> {
                tiLegsContainersModel.setGrossWeight(convertToWeightNumberFormat(tiLegsContainersModel.getGrossWeight(), v1TenantSettingsResponse));
                tiLegsContainersModel.setNetWeight(convertToWeightNumberFormat(tiLegsContainersModel.getNetWeight(), v1TenantSettingsResponse));
                tiLegsContainersModel.setVolume(convertToVolumeNumberFormat(tiLegsContainersModel.getVolume(), v1TenantSettingsResponse));
            });
            legsDictionary.put(ReportConstants.TI_CONTAINERS, tiLegsContainersModels);
        }
    }
}
