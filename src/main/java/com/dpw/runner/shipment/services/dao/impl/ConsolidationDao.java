package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.LicenseContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.ConsoleBookingRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.response.consolidation.IConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.entity.response.consolidation.IShipmentContainerLiteResponse;
import com.dpw.runner.shipment.services.entity.response.consolidation.IShipmentLiteResponse;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ConsolidationDao implements IConsolidationDetailsDao {
    public static final String CONSUMED = "Consumed";
    public static final String UNUSED = "Unused";
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
    private IV1Service v1Service;

    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private EntityManager entityManager;

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync) {
        return save(consolidationDetails, fromV1Sync, false);
    }

    @Override
    public ConsolidationDetails save(ConsolidationDetails consolidationDetails, boolean fromV1Sync, boolean creatingFromDgShipment) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(consolidationDetails), Constants.CONSOLIDATION, LifecycleHooks.ON_CREATE, false);
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
        onSave(consolidationDetails, errors, oldConsole, fromV1Sync, creatingFromDgShipment);
        return consolidationDetails;
    }

    @Override
    public ConsolidationDetails update(ConsolidationDetails consolidationDetails, boolean fromV1Sync) {
        return update(consolidationDetails, fromV1Sync, false);
    }

    @Override
    public ConsolidationDetails update(ConsolidationDetails consolidationDetails, boolean fromV1Sync, boolean updatingFromDgShipment) {
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
        onSave(consolidationDetails, errors, oldConsole, fromV1Sync, updatingFromDgShipment);
        return consolidationDetails;
    }

    private void onSave(ConsolidationDetails consolidationDetails, Set<String> errors, ConsolidationDetails oldConsole, boolean fromV1Sync, boolean creatingFromDgShipment) {
        errors.addAll(applyConsolidationValidations(consolidationDetails, creatingFromDgShipment, fromV1Sync));
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        validateTransportModeAndCarrierDetails(consolidationDetails);
        // assign consolidation bol to mawb field as well
        if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
            consolidationDetails.setMawb(consolidationDetails.getBol());
        }
        if (!fromV1Sync && consolidationDetails.getTransportMode() != null
                && consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR))
            consolidationMAWBCheck(consolidationDetails, oldConsole != null ? oldConsole.getMawb() : null);

        if(!Objects.isNull(oldConsole)) {
            consolidationDetails.setCreatedAt(oldConsole.getCreatedAt());
            consolidationDetails.setCreatedBy(oldConsole.getCreatedBy());
        }
        processOldConsole(consolidationDetails, oldConsole);
        consolidationDetails = consolidationRepository.save(consolidationDetails);
        if (!fromV1Sync && StringUtility.isNotEmpty(consolidationDetails.getMawb()) && StringUtility.isNotEmpty(consolidationDetails.getShipmentType()) && !consolidationDetails.getShipmentType().equalsIgnoreCase(Constants.IMP)) {
            setMawbStock(consolidationDetails);
        }
    }

    private void processOldConsole(ConsolidationDetails consolidationDetails, ConsolidationDetails oldConsole) {
        if (!Objects.isNull(oldConsole)
                && (!Objects.equals(consolidationDetails.getInterBranchConsole(), oldConsole.getInterBranchConsole()) || !Objects.equals(consolidationDetails.getOpenForAttachment(), oldConsole.getOpenForAttachment()))
                && (Boolean.FALSE.equals(consolidationDetails.getInterBranchConsole()) || Boolean.FALSE.equals(consolidationDetails.getOpenForAttachment()))) {
            if (Boolean.FALSE.equals(consolidationDetails.getInterBranchConsole())) {
                var interBranchShipment = consolidationDetails.getShipmentsList().stream().filter(c -> !Objects.equals(c.getTenantId(), TenantContext.getCurrentTenant())).toList().isEmpty();
                if (!interBranchShipment)
                    throw new ValidationException(ErrorConstants.VALIDATE_INTER_BRANCH_CONSOLE);
            }
            consoleShipmentMappingDao.deletePendingStateByConsoleId(consolidationDetails.getId());
        }
    }

    private void validateTransportModeAndCarrierDetails(ConsolidationDetails consolidationDetails) {
        if (consolidationDetails.getTransportMode() != null && consolidationDetails.getCarrierDetails() != null) {
            LocalDateTime eta = consolidationDetails.getCarrierDetails().getEta();
            LocalDateTime etd = consolidationDetails.getCarrierDetails().getEtd();
            if (consolidationDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                //for air consolidation, ETA can be less than ETD and difference should not be more than 24 hours
                validateETA(eta, etd);
            } else {
                //for other transport modes other than AIR, ETA cannot be less than ETD
                if (eta != null && etd != null && eta.isBefore(etd)) {
                    throw new ValidationException("ETA should not be less than ETD");
                }
            }
        }
    }

    private void validateETA(LocalDateTime eta, LocalDateTime etd) {
        if (eta != null && etd != null && eta.isBefore(etd)) {
            Duration duration = Duration.between(eta, etd);
            if (Math.abs(duration.toHours()) > 24) {
                throw new ValidationException("Difference between ETA and ETD should not be more than 24 hours");
            }
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

    public Optional<ConsolidationDetails> findByGuid (UUID guid) {
        return consolidationRepository.findByGuid(guid);
    }

    public List<ConsolidationDetails> findByBol (String bol) {
        return consolidationRepository.findByBol(bol, TenantContext.getCurrentTenant());
    }

    @Override
    public List<ConsolidationDetails> findByReferenceNumber(String ref) {
        return consolidationRepository.findByReferenceNumber(ref, TenantContext.getCurrentTenant());
    }

    @Override
    public Long findMaxId() {
        return consolidationRepository.findMaxId();
    }

    private boolean checkForNonAirDGFlag(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForNonDGConsoleAndAirDGFlag(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(checkForNonAirDGFlag(request, shipmentSettingsDetails))
            return false;
        return !Boolean.TRUE.equals(request.getHazardous());
    }

    private boolean checkForDGConsoleAndAirDGFlag(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if(checkForNonAirDGFlag(request, shipmentSettingsDetails))
            return false;
        return Boolean.TRUE.equals(request.getHazardous());
    }

    public Set<String> applyConsolidationValidations(ConsolidationDetails request, boolean creatingFromDgShipment, boolean fromV1Sync) {
        Set<String> errors = new LinkedHashSet<>();
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            addAirCargoSecurityValidationsErrors(request, creatingFromDgShipment, fromV1Sync, errors);
        } else {
            addNonDgValidationsErrors(request, creatingFromDgShipment, fromV1Sync, shipmentSettingsDetails, errors);
        }

        // Container Number can not be repeated
        addContainerNumberValidationErrors(request, errors);

        // MBL number must be unique
        addMBLNumberValidationErrors(request, errors);

        // Duplicate party types not allowed
        addPartyTypeValidationErrors(request, errors);

        // Shipment restricted unlocations validation
        addUnLocationValidationErrors(request, shipmentSettingsDetails, errors);

        // Reference No can not be repeated
        addReferenceNumberValidationErrors(request, errors);

        addCarrierDetailsValidationErrors(request, shipmentSettingsDetails, errors);

        return errors;
    }

    private void addCarrierDetailsValidationErrors(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        if((shipmentSettingsDetails.getConsolidationLite() == null || !shipmentSettingsDetails.getConsolidationLite().booleanValue())
                && (request.getCarrierDetails() == null || isStringNullOrEmpty(request.getCarrierDetails().getOrigin()) || isStringNullOrEmpty(request.getCarrierDetails().getDestination()))) {
            errors.add("First load or Last Discharge can not be null.");
        }
    }

    private void addContainerNumberValidationErrors(ConsolidationDetails request, Set<String> errors) {
        if (request.getContainersList() != null && !request.getContainersList().isEmpty()) {
            HashSet<String> hashSet = new HashSet<>();
            for (Containers containers : request.getContainersList()) {
                if (!isStringNullOrEmpty(containers.getContainerNumber())) {
                    if (hashSet.contains(containers.getContainerNumber())) {
                        errors.add("Container Number cannot be same for two different containers");
                        break;
                    } else
                        hashSet.add(containers.getContainerNumber());
                }
            }
        }
    }

    private void addReferenceNumberValidationErrors(ConsolidationDetails request, Set<String> errors) {
        if(!isStringNullOrEmpty(request.getReferenceNumber())) {
            List<ConsolidationDetails> consolidationDetails = findByReferenceNumber(request.getReferenceNumber());
            if(!consolidationDetails.isEmpty() && (request.getId() == null || consolidationDetails.get(0).getId().longValue() != request.getId().longValue())) {
                errors.add("Consolidation with ReferenceNo " + request.getReferenceNumber() + " already exists.");
            }
        }
    }

    private void addUnLocationValidationErrors(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getRestrictedLocationsEnabled()) && request.getCarrierDetails() != null) {
            if (request.getShipmentType().equals(Constants.DIRECTION_EXP)) {
                addEXPValidationErrors(request, shipmentSettingsDetails, errors);
            } else if (request.getShipmentType().equals(Constants.IMP)) {
                addIMPValidationsErrors(request, shipmentSettingsDetails, errors);
            }
        }
    }

    private void addIMPValidationsErrors(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        String unLoc;
        unLoc = request.getCarrierDetails().getDestinationPort();
        if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
            errors.add("Value entered for Discharge Port is not allowed or invalid");
        }
        unLoc = request.getCarrierDetails().getDestination();
        if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
            errors.add("Value entered for Last Discharge is not allowed or invalid");
        }
    }

    private void addEXPValidationErrors(ConsolidationDetails request, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        String unLoc;
        unLoc = request.getCarrierDetails().getOriginPort();
        if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
            errors.add("Value entered for Loading Port is not allowed or invalid");
        }
        unLoc = request.getCarrierDetails().getOrigin();
        if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
            errors.add("Value entered for First Load is not allowed or invalid");
        }
    }

    private void addPartyTypeValidationErrors(ConsolidationDetails request, Set<String> errors) {
        if (request.getConsolidationAddresses() != null && !request.getConsolidationAddresses().isEmpty()) {
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
    }

    private void addMBLNumberValidationErrors(ConsolidationDetails request, Set<String> errors) {
        if(!isStringNullOrEmpty(request.getBol())) {
            List<ConsolidationDetails> consolidationDetails = findByBol(request.getBol());
            if(checkSameMblExists(consolidationDetails, request)) {
                errors.add(String.format("The MBL Number %s is already used. Please use a different MBL Number", request.getBol()));
            }
        }
    }

    private void addNonDgValidationsErrors(ConsolidationDetails request, boolean creatingFromDgShipment, boolean fromV1Sync, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        // Non dg consolidation validations
        if(checkForNonDGConsoleAndAirDGFlag(request, shipmentSettingsDetails)) {
            // Non dg Consolidations can not have dg shipments
            boolean isDGShipmentAttached = checkContainsDGShipment(request, false);
            if (isDGShipmentAttached) {
                errors.add("The consolidation contains DG shipment. Marking the consolidation as non DG is not allowed");
            }

            // Non dg Consolidations can not have dg packs
            if(request.getPackingList() != null && !isDGShipmentAttached && checkContainsDGPackage(request)) {
                errors.add("The consolidation contains DG package. Marking the consolidation as non DG is not allowed");
            }
        }
        // Dg consolidation validations
        if (!fromV1Sync && checkForDGConsoleAndAirDGFlag(request, shipmentSettingsDetails)) {

            // Non dg user cannot save dg consolidation
            if (! LicenseContext.isDgAirLicense())
                errors.add("You don't have permission to update DG Consolidation");

            // Dg consolidation must have at least one dg shipment
            boolean containsDgShipment = checkContainsDGShipment(request, creatingFromDgShipment);
            if (!containsDgShipment && !creatingFromDgShipment)
                errors.add("Consolidation cannot be marked as DG. Please attach at least one DG Shipment.");
        }
    }

    private void addAirCargoSecurityValidationsErrors(ConsolidationDetails request, boolean creatingFromDgShipment, boolean fromV1Sync, Set<String> errors) {
        if (!fromV1Sync && !CommonUtils.checkAirSecurityForConsolidation(request)) {
            errors.add("You don't have Air Security permission to create or update AIR EXP Consolidation.");
        }
        // Non dg consolidation validations
        if (!Boolean.TRUE.equals(request.getHazardous())) {
            // Non dg Consolidations can not have dg shipments
            boolean isDGShipmentAttached = checkContainsDGShipment(request, false);
            if (isDGShipmentAttached) {
                errors.add("The consolidation contains DG shipment. Marking the consolidation as non DG is not allowed");
            }

            // Non dg Consolidations can not have dg packs
            if(request.getPackingList() != null && !isDGShipmentAttached && checkContainsDGPackage(request)) {
                errors.add("The consolidation contains DG package. Marking the consolidation as non DG is not allowed");
            }
        }
        if (!fromV1Sync && Boolean.TRUE.equals(request.getHazardous())) {
            // Dg consolidation must have at least one dg shipment
            boolean containsDgShipment = checkContainsDGShipment(request, creatingFromDgShipment);
            if (!containsDgShipment && !creatingFromDgShipment)
                errors.add("Consolidation cannot be marked as DG. Please attach at least one DG Shipment.");
        }
    }

    public boolean checkSameMblExists(List<ConsolidationDetails> consolidationDetails, ConsolidationDetails request) {
        if(consolidationDetails == null)
            return false;
        if(consolidationDetails.isEmpty())
            return false;
        if(request.getId() == null)
            return true;
        if(consolidationDetails.size() > 1)
            return true;
        return request.getId().longValue() != consolidationDetails.get(0).getId().longValue();
    }

    private void setMawbStock(ConsolidationDetails consolidationDetails) {
        List<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findByMawbNumber(consolidationDetails.getMawb());
        if(mawbStocksLinks != null && !mawbStocksLinks.isEmpty()) {
            MawbStocksLink res = mawbStocksLinks.get(0);
            if(!res.getStatus().equalsIgnoreCase(CONSUMED)) {
                res.setEntityId(consolidationDetails.getId());
                res.setEntityType(Constants.CONSOLIDATION);
                res.setShipConsNumber(consolidationDetails.getConsolidationNumber());
                res.setStatus(CONSUMED);
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
            res.setNextMawbNumber(mawbStocksLinkDao.assignNextMawbNumber(parentId));
            mawbStocksDao.save(res);
        }
    }

    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode, String agentType) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("AirlineCode"), "=", mawbAirlineCode));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("HasAirPort"), "=", 1));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        carrierListObject.setType(agentType);
        return v1Service.fetchCarrierMasterData(carrierListObject, false);
    }

    private void consolidationMAWBCheck(ConsolidationDetails consolidationRequest, String oldMawb) {
        if (StringUtility.isEmpty(consolidationRequest.getMawb())) {
            if(!consolidationRequest.getShipmentType().equals(Constants.IMP)) {
                mawbStocksLinkDao.deLinkExistingMawbStockLink(oldMawb);
            }
            return;
        }
        if (!Objects.equals(consolidationRequest.getMawb(), oldMawb) && !consolidationRequest.getShipmentType().equals(Constants.IMP)) {
            mawbStocksLinkDao.deLinkExistingMawbStockLink(oldMawb);
        }

        if (Boolean.FALSE.equals(isMAWBNumberValid(consolidationRequest.getMawb())))
            throw new ValidationException("Please enter a valid MAWB number.");

        CarrierResponse correspondingCarrier = getCorrespondingCarrier(consolidationRequest, oldMawb);

        if (consolidationRequest.getShipmentType().equals(Constants.IMP)) {
            return;
        }


        boolean isMAWBNumberExist = false;

        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", consolidationRequest.getMawb(), "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = mawbStocksLinkDao.findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        MawbStocksLink mawbStocksLink = null;

        if (!mawbStocksLinkPage.getContent().isEmpty()){
            isMAWBNumberExist = true;
            mawbStocksLink = mawbStocksLinkPage.getContent().get(0);
        }

        if (isMAWBNumberExist){
            if (mawbStocksLink.getStatus().equals(CONSUMED) && !Objects.equals(consolidationRequest.getId(), mawbStocksLink.getEntityId())) {
                throw new ValidationException("The MAWB number entered is already consumed. Please enter another MAWB number.");
            }
        } else {
            createNewMAWBEntry(consolidationRequest, correspondingCarrier != null ? correspondingCarrier.getItemValue() : consolidationRequest.getCarrierDetails().getShippingLine());
        }
    }

    private CarrierResponse getCorrespondingCarrier(ConsolidationDetails consolidationRequest, String oldMawb) {
        CarrierResponse correspondingCarrier = null;
        if(consolidationRequest.getCarrierDetails() == null || StringUtility.isEmpty(consolidationRequest.getCarrierDetails().getShippingLine()) ||
            !Objects.equals(consolidationRequest.getMawb(), oldMawb) ) {
            String mawbAirlineCode = consolidationRequest.getMawb().substring(0, 3);
            V1DataResponse v1DataResponse = fetchCarrierDetailsFromV1(mawbAirlineCode, consolidationRequest.getConsolidationType());
            List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);

            if (carrierDetails == null || carrierDetails.isEmpty())
                throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");

            correspondingCarrier = carrierDetails.get(0);
            if(consolidationRequest.getCarrierDetails() == null || StringUtility.isEmpty(consolidationRequest.getCarrierDetails().getShippingLine())) {

                if (consolidationRequest.getCarrierDetails() == null)
                    consolidationRequest.setCarrierDetails(new CarrierDetails());

                consolidationRequest.getCarrierDetails().setShippingLine(correspondingCarrier.getItemValue());
            }
        }
        return correspondingCarrier;
    }

    private void createNewMAWBEntry(ConsolidationDetails consolidationRequest, String shippingLine) {
        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setAirLinePrefix(shippingLine);
        mawbStocks.setCount("1");
        mawbStocks.setAvailableCount("1");
        mawbStocks.setStartNumber(Long.valueOf(consolidationRequest.getMawb().substring(4, 10)));
        mawbStocks.setFrom(consolidationRequest.getMawb());
        mawbStocks.setTo(consolidationRequest.getMawb());
        mawbStocks.setMawbNumber(consolidationRequest.getMawb());
        mawbStocks.setStatus(UNUSED);
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
            entryForMawbStocksLinkRow.setStatus(UNUSED);
            mawbStocksLinkDao.save(entryForMawbStocksLinkRow);
        }
    }

    public Boolean isMAWBNumberValid(String masterBill) {
        boolean mAWBNumberValidity = true;
        if (masterBill.length() == 12) {
            String mawbSeqNum = masterBill.substring(4, 11);
            String checkDigit = masterBill.substring(11, 12);
            long imawbSeqNum;
            long icheckDigit;
            if (areAllCharactersDigits(masterBill, 4, 12)) {
                imawbSeqNum = Long.parseLong(mawbSeqNum);
                icheckDigit = Long.parseLong(checkDigit);
                if (imawbSeqNum % 7 != icheckDigit)
                    mAWBNumberValidity = false;
            } else mAWBNumberValidity = false;
        } else mAWBNumberValidity = false;
        return mAWBNumberValidity;
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

    @Override
    public int updateConsoleBookingFields(ConsoleBookingRequest request){
        return consolidationRepository.updateConsoleBookingFields(request.getGuid(), request.getBookingId(), request.getBookingStatus(), request.getBookingNumber());
    }

    @Transactional
    public void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate) {consolidationRepository.saveCreatedDateAndUser(id, createdBy, createdDate);}

    public String getConsolidationNumberFromId(Long id) {
        return consolidationRepository.getConsolidationNumberFromId(id);
    }

    @Override
    public List<ConsolidationDetails> findConsolidationsByGuids(Set<UUID> guids) {
        return consolidationRepository.findConsolidationsByGuids(guids);
    }

    @Override
    public List<ConsolidationDetails> findConsolidationsByIds(Set<Long> ids) {
        return consolidationRepository.findConsolidationsByIds(ids);
    }

    @Override
    public ConsolidationDetails findConsolidationsById(Long id) {
        return consolidationRepository.getConsolidationFromId(id);
    }

    @Override
    public List<ConsolidationDetailsProjection> findMblNumberInDifferentTenant(String mblNumber) {
        return consolidationRepository.findMblNumberInDifferentTenant(mblNumber, TenantContext.getCurrentTenant());
    }

    @Override
    public Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable) {
        return consolidationRepository.getIdWithPendingActions(shipmentRequestedType, pageable);
    }

    @Override
    public List<ConsolidationDetails> findBySourceGuid(UUID guid) {
        return consolidationRepository.findBySourceGuid(guid);
    }

    @Override
    @Transactional
    public void entityDetach(List<ConsolidationDetails> consolidationDetails) {
        for(ConsolidationDetails consolidationDetail : consolidationDetails) {
            entityManager.detach(consolidationDetail);
        }
    }

    @Override
    @Transactional
    public Optional<ConsolidationDetails> findConsolidationByIdWithQuery(Long id) {
        return consolidationRepository.findConsolidationByIdWithQuery(id);
    }

    @Override
    @Transactional
    public void saveIsTransferredToReceivingBranch(Long id, Boolean entityTransferred) {
        consolidationRepository.saveIsTransferredToReceivingBranch(id, entityTransferred);
    }

    @Override
    @Transactional
    public void updateIsAcceptedTriangulationPartner(Long consolidationId, Long triangulationPartner, Boolean isAccepted) {
        consolidationRepository.updateIsAcceptedTriangulationPartner(consolidationId, triangulationPartner, isAccepted);
    }


    @Override
    public List<IShipmentLiteResponse> findIShipmentsByConsolidationIds(
        List<Long> consolidationIDs) {
        return consolidationRepository.findIShipmentsByConsolidationIds(consolidationIDs);
    }

    @Override
    public List<IShipmentContainerLiteResponse> findShipmentDetailsWithContainersByConsolidationIds(
        List<Long> consolidationIDs) {
        return consolidationRepository.findShipmentDetailsWithContainersByConsolidationIds(consolidationIDs);
    }

    @Override
    public Page<IConsolidationDetailsResponse> findAllLiteConsol(Specification<ConsolidationDetails> spec, Pageable pageable) {
        return consolidationRepository.findAllLiteConsol(spec, pageable);
    }

    @Override
    @Transactional
    public Optional<ConsolidationDetails> findConsolidationByGuidWithQuery(UUID guid) {
        return consolidationRepository.findConsolidationByGuidWithQuery(guid);
    }

    private boolean checkContainsDGShipment(ConsolidationDetails request, boolean creatingFromDgShipment) {
        if (!CommonUtils.setIsNullOrEmpty(request.getShipmentsList()) && !creatingFromDgShipment) {
            for (ShipmentDetails shipmentDetails : request.getShipmentsList()) {
                if (Boolean.TRUE.equals(shipmentDetails.getContainsHazardous())) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean checkContainsDGPackage(ConsolidationDetails request) {
        for (Packing packing: request.getPackingList()) {
            if(Boolean.TRUE.equals(packing.getHazardous())) {
                return true;
            }
        }
        return false;
    }

}
