package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.CSDModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.AdditionalDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.CarrierDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.RoutingsModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DATE_OF_PRINT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RA_CSD;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGINAL_PRINT_DATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TIME_OF_PRINT;

@Component
public class CSDReport extends IReport{

    private boolean isConsolidation;
    public void setIsConsolidation(boolean isConsolidation){
        this.isConsolidation = isConsolidation;
    }
    private static final String TIME_FORMAT = "HHmm";
    private static final String DATE_FORMAT = "ddMMMyy";

    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        CSDModel csdModel = (CSDModel) getDocumentModel(id);
        return populateDictionary(csdModel);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) throws RunnerException {
        CSDModel csdModel = new CSDModel();
        csdModel.setUsersDto(UserContext.getUser());
        if(isConsolidation){
            csdModel.setConsolidationModel(getConsolidation(id));
            csdModel.setAwb(getMawb(id, true));
        }else{
            csdModel.setShipmentModel(getShipment(id));
            csdModel.setAwb(getHawb(id));
        }
        return csdModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        CSDModel csdModel = (CSDModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();

        CarrierDetailModel carrierModel;

        populateUserFields(csdModel.getUsersDto(), dictionary);
        if(isConsolidation){
            populateConsolidationFields(csdModel.getConsolidationModel(), dictionary);
            populateRaKcDataConsolidation(dictionary, csdModel.getConsolidationModel());
            dictionary.put(ReportConstants.IS_CONSOLIDATION, true);
            // CarrierDetails
            if(csdModel.getConsolidationModel().getCarrierDetails() != null) {
                carrierModel = csdModel.getConsolidationModel().getCarrierDetails();
                dictionary.put(ReportConstants.TRANSIT_AIRPORTS, getMainCarriageAirPorts(
                        csdModel.getConsolidationModel().getRoutingsList(), carrierModel.getOriginPort(), carrierModel.getDestinationPort()
                ));
            }
            if(!CollectionUtils.isEmpty(csdModel.getConsolidationModel().getScreeningStatus()))
                dictionary.put(ReportConstants.SCREENING_CODES, new HashSet<>(csdModel.getConsolidationModel().getScreeningStatus()));
        }
        else {
            populateShipmentFields(csdModel.getShipmentModel(), dictionary);
            populateRaKcData(dictionary, csdModel.getShipmentModel());
            // Shipment Type tags
            String shipmentType = (Objects.equals(csdModel.getShipmentModel().getJobType(), Constants.SHIPMENT_TYPE_DRT)) ? Constants.DMAWB : Constants.HAWB;
            boolean isDirect = Constants.DMAWB.equals(shipmentType);
            dictionary.put(ReportConstants.IS_DIRECT_SHIPMENT, isDirect);
            dictionary.put(ReportConstants.IS_NON_DIRECT_SHIPMENT, !isDirect);
            // CarrierDetails
            if(csdModel.getShipmentModel().getCarrierDetails() != null) {
                carrierModel = csdModel.getShipmentModel().getCarrierDetails();
                dictionary.put(ReportConstants.TRANSIT_AIRPORTS, getMainCarriageAirPorts(
                        csdModel.getShipmentModel().getRoutingsList(), carrierModel.getOriginPort(), carrierModel.getDestinationPort()
                ));
            }
            AdditionalDetailModel additionalDetailModel = Optional.ofNullable(csdModel.getShipmentModel().getAdditionalDetails()).orElse(new AdditionalDetailModel());
            dictionary.put(ReportConstants.REGULATORY_ENTITY_CATEGORY, additionalDetailModel.getRegulatedEntityCategory());
            if(!CollectionUtils.isEmpty(additionalDetailModel.getScreeningStatus()))
                dictionary.put(ReportConstants.SCREENING_CODES, new HashSet<>(additionalDetailModel.getScreeningStatus()));
        }

        var securityStatus = dictionary.get(ReportConstants.CONSIGNMENT_STATUS);
        if(AwbConstants.EXEMPTION_CARGO_SECURITY_STATUS.equalsIgnoreCase(StringUtility.convertToString(securityStatus)))
            securityStatus = AwbConstants.SPX;
        dictionary.put(ReportConstants.CONSIGNMENT_STATUS, securityStatus);
        dictionary.put(ReportConstants.DEFAULT_RA_NUMBER, getDefaultRANumber());

        dictionary.put(DATE_OF_PRINT, StringUtility.convertToString(ConvertToDPWDateFormat(LocalDateTime.now(), DATE_FORMAT, true)));
        dictionary.put(TIME_OF_PRINT, StringUtility.convertToString(ConvertToDPWDateFormat(LocalDateTime.now(), TIME_FORMAT, true)));

        if (Objects.nonNull(csdModel.getAwb())) {
            dictionary.put(RA_CSD, geteCSDInfo(csdModel.getAwb()));
            dictionary.put(ORIGINAL_PRINT_DATE, getPrintOriginalDate(csdModel.getAwb()));
        }
        return dictionary;
    }

    private String getMainCarriageAirPorts(List<RoutingsModel> routingsModelList, String pol, String pod) {
        if (CollectionUtils.isEmpty(routingsModelList))
            return StringUtility.getEmptyString();
        var airMainCarriageRouting = routingsModelList.stream().filter(i -> ReportConstants.AIR.equalsIgnoreCase(i.getMode()))
                .filter(i -> RoutingCarriage.MAIN_CARRIAGE.equals(i.getCarriage())).toList();
        List<String> airPorts = new ArrayList<>();
        airMainCarriageRouting.forEach(i -> {
            airPorts.add(i.getPol());
            airPorts.add(i.getPod());
        });
        Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(airPorts));
        // Ordering of air ports is important
        var airPortsName = new LinkedHashSet<>(airPorts).stream()
                .filter(i -> !(i.equalsIgnoreCase(pol) || i.equalsIgnoreCase(pod)))
                .map(i -> Optional.ofNullable(unlocationsMap.get(i)).map(UnlocationsResponse::getIataCode).orElse(i))
                .toList();

        return String.join(",", airPortsName);
    }
}
