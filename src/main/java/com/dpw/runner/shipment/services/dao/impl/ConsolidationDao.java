package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ConsolidationDao implements IConsolidationDetailsDao {
    @Autowired
    private IConsolidationRepository consolidationRepository;

    @Autowired
    IShipmentRepository shipmentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Autowired
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private ICarrierDao carrierDao;

    @Autowired
    private IV1Service v1Service;

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(consolidationDetails) , Constants.CONSOLIDATION, LifecycleHooks.ON_CREATE, false);
        ConsolidationDetails oldConsole = null;
        if(consolidationDetails.getId() != null) {
            long id = consolidationDetails.getId();
            Optional<ConsolidationDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Container is null for Id {}", consolidationDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if(consolidationDetails.getShipmentsList() == null) {
                consolidationDetails.setShipmentsList(oldEntity.get().getShipmentsList());
            }
            oldConsole = oldEntity.get();
        }
        onSave(consolidationDetails, errors, oldConsole, fromV1Sync);
        return consolidationDetails;
    }

    @Override
    public ConsolidationDetails update(ConsolidationDetails consolidationDetails, boolean fromV1Sync) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(consolidationDetails) , Constants.CONSOLIDATION, LifecycleHooks.ON_CREATE, false);
        validateLockStatus(consolidationDetails);
        ConsolidationDetails oldConsole = null;
        if(consolidationDetails.getId() != null) {
            long id = consolidationDetails.getId();
            ConsolidationDetails oldEntity = findById(id).get();
            if(consolidationDetails.getShipmentsList() == null) {
                consolidationDetails.setShipmentsList(oldEntity.getShipmentsList());
            }
            oldConsole = oldEntity;
        }
        onSave(consolidationDetails, errors, oldConsole, fromV1Sync);
        return consolidationDetails;
    }

    private void onSave(ConsolidationDetails consolidationDetails, Set<String> errors, ConsolidationDetails oldConsole, boolean fromV1Sync) {
        errors.addAll(applyConsolidationValidations(consolidationDetails, oldConsole));
        if (!errors.isEmpty())
            throw new ValidationException(errors.toString());
        if (consolidationDetails.getTransportMode() != null && consolidationDetails.getCarrierDetails() != null) {
            LocalDateTime eta = consolidationDetails.getCarrierDetails().getEta();
            LocalDateTime etd = consolidationDetails.getCarrierDetails().getEtd();
            if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                //for air consolidation, ETA can be less than ETD and difference should not be more than 24 hours
                if (eta != null && etd != null && eta.isBefore(etd)) {
                    Duration duration = Duration.between(eta, etd);
                    if (Math.abs(duration.toHours()) > 24) {
                        throw new ValidationException("Difference between ETA and ETD should not be more than 24 hours");
                    }
                }
            } else {
                //for other transport modes other than AIR, ETA cannot be less than ETD
                if (eta != null && etd != null && eta.isBefore(etd)) {
                    throw new ValidationException("ETA should not be less than ETD");
                }
            }
        }
        if (!fromV1Sync && consolidationDetails.getTransportMode() != null
                && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && consolidationDetails.getMawb() != null
                && (oldConsole == null || oldConsole.getMawb() == null || !oldConsole.getMawb().equalsIgnoreCase(consolidationDetails.getMawb())))
            consolidationMAWBCheck(consolidationDetails);
        consolidationDetails = consolidationRepository.save(consolidationDetails);
        if (!fromV1Sync && StringUtility.isNotEmpty(consolidationDetails.getMawb()) && StringUtility.isNotEmpty(consolidationDetails.getShipmentType()) && !consolidationDetails.getShipmentType().equalsIgnoreCase(Constants.IMP)) {
            setMawbStock(consolidationDetails);
        }
    }

    @Override
    public Page<ConsolidationDetails> findAll(Specification<ConsolidationDetails> spec, Pageable pageable) {
        return consolidationRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ConsolidationDetails> findById(Long id) {
        return consolidationRepository.findById(id);
    }

    @Override
    public void delete(ConsolidationDetails consolidationDetails) {
        validateLockStatus(consolidationDetails);
        consolidationRepository.delete(consolidationDetails);
    }

    private void validateLockStatus(ConsolidationDetails consolidationDetails) throws ValidationException {
        if(consolidationDetails.getIsLocked() != null && consolidationDetails.getIsLocked()) {
            throw new ValidationException(ConsolidationConstants.CONSOLIDATION_LOCKED);
        }
    }
    
    public List<ConsolidationDetails> saveAll(List<ConsolidationDetails> consolidationDetails)
    {
        List<ConsolidationDetails> res = new ArrayList<>();
        for(ConsolidationDetails req : consolidationDetails){
            req = save(req, false);
            res.add(req);
        }
        return res;
    }

    public Optional<ShipmentDetails> findShipmentById(Long shipmentId) {
        return shipmentRepository.findById(shipmentId);
    }

    public Optional<ConsolidationDetails> findByGuid (UUID guid) {
        return consolidationRepository.findByGuid(guid);
    }

    public List<ConsolidationDetails> findByBol (String bol) {
        return consolidationRepository.findByBol(bol);
    }

    @Override
    public List<ConsolidationDetails> findByReferenceNumber(String ref) {
        return consolidationRepository.findByReferenceNumber(ref);
    }

    @Override
    public Long findMaxId() {
        return consolidationRepository.findMaxId();
    }

    private Set<String> applyConsolidationValidations(ConsolidationDetails request, ConsolidationDetails oldEntity) {
        Set<String> errors = new LinkedHashSet<>();
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();

        // Container Number can not be repeated
        if (request.getContainersList() != null && request.getContainersList().size() > 0) {
            HashSet<String> hashSet = new HashSet<>();
            for (Containers containers : request.getContainersList()) {
                if (!IsStringNullOrEmpty(containers.getContainerNumber())) {
                    if (hashSet.contains(containers.getContainerNumber())) {
                        errors.add("Container Number cannot be same for two different containers");
                        break;
                    } else
                        hashSet.add(containers.getContainerNumber());
                }
            }
        }

        // MBL number must be unique
        if(!IsStringNullOrEmpty(request.getBol())) {
            List<ConsolidationDetails> consolidationDetails = findByBol(request.getBol());
            if(consolidationDetails != null && consolidationDetails.size() > 0) {
                if(request.getId() == null || (request.getId().longValue() != consolidationDetails.get(0).getId().longValue())) {
                    errors.add(String.format("The MBL Number %s is already used. Please use a different MBL Number", request.getBol()));
                }
            }
        }

        // Duplicate party types not allowed
        if (request.getConsolidationAddresses() != null && request.getConsolidationAddresses().size() > 0) {
            HashSet<String> partyTypes = new HashSet<>();
            HashSet<String> duplicatePartyTypes = new HashSet<>();
            for (Parties item : request.getConsolidationAddresses()) {
                if (partyTypes.contains(item.getType())) {
                    duplicatePartyTypes.add(item.getType());
                } else {
                    partyTypes.add(item.getType());
                }
            }
            if (!duplicatePartyTypes.isEmpty()) {
                String types = String.join(", ", duplicatePartyTypes);

                String message = (duplicatePartyTypes.size() == 1) ? " is a duplicate Party Type." : " are duplicate Party Types.";
                errors.add(types + message);
            }
        }

        // Shipment restricted unlocations validation
        if (shipmentSettingsDetails.getRestrictedLocationsEnabled() && request.getCarrierDetails() != null) {
            String unLoc = null;
            if (request.getShipmentType().equals(Constants.DIRECTION_EXP)) {
                unLoc = request.getCarrierDetails().getOriginPort();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Loading Port is not allowed or invalid");
                }
                unLoc = request.getCarrierDetails().getOrigin();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for First Load is not allowed or invalid");
                }
            } else if (request.getShipmentType().equals(Constants.IMP)) {
                unLoc = request.getCarrierDetails().getDestinationPort();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Discharge Port is not allowed or invalid");
                }
                unLoc = request.getCarrierDetails().getDestination();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Last Discharge is not allowed or invalid");
                }
            }
        }

        // Reference No can not be repeated
        if(!IsStringNullOrEmpty(request.getReferenceNumber())) {
            List<ConsolidationDetails> consolidationDetails = findByReferenceNumber(request.getReferenceNumber());
            if(!consolidationDetails.isEmpty() && (request.getId() == null || consolidationDetails.get(0).getId().longValue() != request.getId().longValue())) {
                errors.add("Consolidation with ReferenceNo " + request.getReferenceNumber() + " already exists.");
            }
        }

        if((shipmentSettingsDetails.getConsolidationLite() == null || !shipmentSettingsDetails.getConsolidationLite().booleanValue())
                && IsStringNullOrEmpty(request.getCarrierDetails().getOrigin()) && IsStringNullOrEmpty(request.getCarrierDetails().getDestination())) {
            errors.add("First load or Last Discharge can not be null.");
        }

        return errors;
    }

    private void setMawbStock(ConsolidationDetails consolidationDetails) {
        List<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findByMawbNumber(consolidationDetails.getMawb());
        if(mawbStocksLinks != null && mawbStocksLinks.size() > 0) {
            MawbStocksLink res = mawbStocksLinks.get(0);
            if(!res.getStatus().equalsIgnoreCase("Consumed")) {
                res.setEntityId(consolidationDetails.getId());
                res.setEntityType(Constants.CONSOLIDATION);
                res.setStatus("Consumed");
                mawbStocksLinkDao.save(res);
                setAvaliableCount(res.getParentId());
            }
        }
    }

    private void setAvaliableCount(Long parentId) {
        Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(parentId);
        if(!mawbStocks.isEmpty()) {
            MawbStocks res = mawbStocks.get();
            res.setAvailableCount(String.valueOf(Integer.parseInt(res.getAvailableCount() != null ? res.getAvailableCount() : "0") - 1));
            res.setNextMawbNumber(assignNextMawbNumber(parentId));
            mawbStocksDao.save(res);
        }
    }

    private String assignNextMawbNumber(Long parentId) {
        ListCommonRequest listCommonRequest;
        listCommonRequest = CommonUtils.andCriteria("parentId", parentId, "=", null);
        CommonUtils.andCriteria("status", "Unused", "=", listCommonRequest);
        listCommonRequest.setSortRequest(SortRequest.builder()
                .fieldName("seqNumber")
                .order("DESC")
                .build());
        Pair<Specification<MawbStocksLink>, Pageable> pair = fetchData(listCommonRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findAll(pair.getLeft(), pair.getRight());
        if(!mawbStocksLinks.isEmpty()) {
            return mawbStocksLinks.get().toList().get(0).getMawbNumber();
        }
        return null;
    }

    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.addAll(List.of(List.of("AirlineCode"), "=", mawbAirlineCode));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);
        return response;
    }

    private void consolidationMAWBCheck(ConsolidationDetails consolidationRequest) {
        if (StringUtility.isEmpty(consolidationRequest.getMawb())) {
            return;
        }

        if (!isMAWBNumberValid(consolidationRequest.getMawb()))
            throw new ValidationException("Please enter a valid MAWB number.");

        String mawbAirlineCode = consolidationRequest.getMawb().substring(0, 3);
        V1DataResponse v1DataResponse = fetchCarrierDetailsFromV1(mawbAirlineCode);
        List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);

        if (carrierDetails == null || carrierDetails.size()==0)
            throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");

        CarrierResponse correspondingCarrier = carrierDetails.get(0);

        Boolean isMAWBNumberExist = false;

        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", consolidationRequest.getMawb(), "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = mawbStocksLinkDao.findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        MawbStocksLink mawbStocksLink = null;
        if (!mawbStocksLinkPage.getContent().isEmpty()){
            mawbStocksLink = mawbStocksLinkPage.getContent().get(0);
            isMAWBNumberExist = true;
        }

        if(consolidationRequest.getCarrierDetails() == null)
            consolidationRequest.setCarrierDetails(new CarrierDetails());

        consolidationRequest.getCarrierDetails().setShippingLine(correspondingCarrier.getItemValue());

        if (consolidationRequest.getShipmentType().equals(Constants.IMP)) {
            return;
        }

        if (isMAWBNumberExist){
            if (mawbStocksLink.getStatus().equals("Consumed") && !mawbStocksLink.getEntityId().equals(consolidationRequest.getId())) {
                throw new ValidationException("The MAWB number entered is already consumed. Please enter another MAWB number.");
            }
        } else {
                createNewMAWBEntry(consolidationRequest);
        }
    }

    private void createNewMAWBEntry(ConsolidationDetails consolidationRequest) {
        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setAirLinePrefix(consolidationRequest.getCarrierDetails().getShippingLine());
        mawbStocks.setCount("1");
        mawbStocks.setAvailableCount("1");
        mawbStocks.setStartNumber(Long.valueOf(consolidationRequest.getMawb().substring(4, 10)));
        mawbStocks.setFrom(consolidationRequest.getMawb());
        mawbStocks.setTo(consolidationRequest.getMawb());
        mawbStocks.setMawbNumber(consolidationRequest.getMawb());
        mawbStocks.setStatus("Unused");
        if(consolidationRequest.getBorrowedFrom()!=null){
            mawbStocks.setBorrowedFrom(consolidationRequest.getBorrowedFrom().getOrgCode());
            if(consolidationRequest.getBorrowedFrom().getOrgData() != null && consolidationRequest.getBorrowedFrom().getOrgData().containsKey("FullName")) {
                String name = (String) consolidationRequest.getBorrowedFrom().getOrgData().get("FullName");
                mawbStocks.setBorrowedFromFullName(name);
            }
        }
        mawbStocks.setHomePort(consolidationRequest.getCarrierDetails().getOriginPort());
        mawbStocks = mawbStocksDao.save(mawbStocks);

        if (mawbStocks.getId() != null) {
            var entryForMawbStocksLinkRow = new MawbStocksLink();
            entryForMawbStocksLinkRow.setParentId(mawbStocks.getId());
            entryForMawbStocksLinkRow.setSeqNumber(consolidationRequest.getMawb().substring(4, 10));
            entryForMawbStocksLinkRow.setMawbNumber(consolidationRequest.getMawb());
            entryForMawbStocksLinkRow.setStatus("Unused");
            entryForMawbStocksLinkRow = mawbStocksLinkDao.save(entryForMawbStocksLinkRow);
        }
    }

    public Boolean isMAWBNumberValid(String masterBill) {
        Boolean MAWBNumberValidity = true;
        if (masterBill.length() == 12) {
            String mawbSeqNum = masterBill.substring(4, 11);
            String checkDigit = masterBill.substring(11, 12);
            Long imawbSeqNum = 0L;
            Long icheckDigit = 0L;
            if (areAllCharactersDigits(masterBill, 4, 12)) {
                imawbSeqNum = Long.valueOf(mawbSeqNum);
                icheckDigit = Long.valueOf(checkDigit);
                if (imawbSeqNum % 7 != icheckDigit)
                    MAWBNumberValidity = false;
            } else MAWBNumberValidity = false;
        } else MAWBNumberValidity = false;
        return MAWBNumberValidity;
    }

    private boolean areAllCharactersDigits(String input, int startIndex, int endIndex) {
        String substring = input.substring(startIndex, endIndex);
        for (int i = 0; i < substring.length(); i++) {
            if (!Character.isDigit(substring.charAt(i))) {
                return false;
            }
        }
        return true;
    }

}
