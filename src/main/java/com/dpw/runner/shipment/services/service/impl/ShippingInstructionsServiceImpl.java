package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionEntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShippingInstructionRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@Slf4j
public class ShippingInstructionsServiceImpl implements IShippingInstructionsService {

    @Autowired
    private IShippingInstructionRepository repository;

    @Autowired
    ICarrierBookingDao carrierBookingDao;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private JsonHelper jsonHelper;

    public ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest info) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(info, ShippingInstruction.class);
        validateFetchAndSetSI(shippingInstruction);

        shippingInstruction.setStatus(ShippingInstructionStatus.Draft.name());
        ShippingInstruction savedInfo = repository.save(shippingInstruction);
        return jsonHelper.convertValue(savedInfo, ShippingInstructionResponse.class);
    }

    private void validateFetchAndSetSI(ShippingInstruction shippingInstruction) {
        validateSIRequest(shippingInstruction);
        if (shippingInstruction.getEntityType().toString().equalsIgnoreCase(ShippingInstructionEntityType.CARRIER_BOOKING.name())) {
            Optional<CarrierBooking> carrierBooking = carrierBookingDao.findById(shippingInstruction.getEntityId());
            Optional<ConsolidationDetails> consolidationDetail;
            if (carrierBooking.isPresent()) {
                consolidationDetail = getConsolidationDetail(carrierBooking.get().getEntityId());
                consolidationDetail.ifPresent(consolidationDetails -> fillDetailsFromConsol(shippingInstruction, consolidationDetails));
                populateHeaderSection(shippingInstruction, carrierBooking.get());
            }
        } else if (shippingInstruction.getEntityType().toString().equalsIgnoreCase(ShippingInstructionEntityType.CONSOLIDATION.name())) {
            Optional<ConsolidationDetails> consolidationDetail = getConsolidationDetail(shippingInstruction.getEntityId());
            consolidationDetail.ifPresent(consolidationDetails -> fillDetailsFromConsol(shippingInstruction, consolidationDetails));
        } else {
            throw new ValidationException("Invalid value of Shipping Instruction Type");
        }
    }

    private Optional<ConsolidationDetails> getConsolidationDetail(Long id) {
        return consolidationDetailsDao.findById(id);
    }

    public ShippingInstructionResponse getShippingInstructionsById(Long id) {
        Optional<ShippingInstruction> shippingInstruction = repository.findById(id);
        if (shippingInstruction.isEmpty()) {
            log.debug("SI is null for Id {}", id);
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
    }

    public ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest updatedInfo) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(updatedInfo, ShippingInstruction.class);
        validateFetchAndSetSI(shippingInstruction);
        ShippingInstruction information = repository.save(shippingInstruction);
        return jsonHelper.convertValue(information, ShippingInstructionResponse.class);
    }

    public void deleteShippingInstructions(Long id) {
        repository.deleteById(id);
    }

    private void fillDetailsFromConsol(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        setSailingInfoAndCutoff(shippingInstruction, consolidationDetails);
    }

    private void setSailingInfoAndCutoff(ShippingInstruction shippingInstruction, ConsolidationDetails consolidationDetails) {
        if (shippingInstruction.getSailingInformation() != null) {
            shippingInstruction.getSailingInformation().setCarrierReceiptPlace(consolidationDetails.getCarrierDetails().getOrigin());
            shippingInstruction.getSailingInformation().setPol(consolidationDetails.getCarrierDetails().getOriginPort());
            shippingInstruction.getSailingInformation().setPod(consolidationDetails.getCarrierDetails().getDestinationPort());
            shippingInstruction.getSailingInformation().setCarrierDeliveryPlace(consolidationDetails.getCarrierDetails().getDestination());
            //    shippingInstruction.getSailingInformation().setCarrier(consolidationDetails.getCarrierDetails().get);
            shippingInstruction.getSailingInformation().setShipInstructionCutoff(consolidationDetails.getShipInstructionCutoff());
            shippingInstruction.getSailingInformation().setVerifiedGrossMassCutoff(consolidationDetails.getVerifiedGrossMassCutoff());
        }
    }

    private void populateHeaderSection(ShippingInstruction shippingInstruction, CarrierBooking carrierBooking) {
        shippingInstruction.setStatus(carrierBooking.getStatus().toString());
        shippingInstruction.setCarrierBookingNo(carrierBooking.getCarrierBookingNo());
        shippingInstruction.setCarrierBlNo(carrierBooking.getCarrierBlNo());
        shippingInstruction.setServiceType(carrierBooking.getServiceType());
    }

    private void validateSIRequest(ShippingInstruction shippingInstruction) {
        if (Integer.parseInt(String.valueOf(shippingInstruction.getNoOfFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNoOfFreightCopies())) < 0) {
            log.info("Validation failed for number of freight copies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid freight copies number!");
        }

        if (Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoFreightCopies())) < 0) {
            log.info("Validation failed for getNonNegoFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid getNonNegoFreightCopies!");
        }

        if (Integer.parseInt(String.valueOf(shippingInstruction.getNoOfUnFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNoOfUnFreightCopies())) < 0) {
            log.info("Validation failed for getNoOfUnFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid un freight copies number!");
        }

        if (Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoUnFreightCopies())) > 100
                || Integer.parseInt(String.valueOf(shippingInstruction.getNonNegoUnFreightCopies())) < 0) {
            log.info("Validation failed for getNonNegoUnFreightCopies for SI id : {}", shippingInstruction.getId());
            throw new ValidationException("Invalid getNonNegoUnFreightCopies!");
        }
    }

}
