package com.dpw.runner.shipment.services.dao.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.getConstrainViolationErrorMessage;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ShipmentSailingScheduleRequest;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.projection.CustomerBookingProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import javax.persistence.EntityManager;
import javax.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@Repository
@Slf4j
public class ShipmentDao implements IShipmentDao {
    public static final String CONSUMED = "Consumed";
    public static final String UNUSED = "Unused";
    @Autowired
    private IShipmentRepository shipmentRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;


    @Autowired
    private IMawbStocksDao mawbStocksDao;

    @Autowired
    private IMawbStocksLinkDao mawbStocksLinkDao;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    private final EntityManager entityManager;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    public ShipmentDao(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    @Override
    public ShipmentDetails save(ShipmentDetails shipmentDetails, boolean fromV1Sync) throws RunnerException {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails), Constants.SHIPMENT, LifecycleHooks.ON_CREATE, false);
        ShipmentDetails oldShipment = null;
        if (shipmentDetails.getId() != null) {
            oldShipment = getAndProcessOldEntity(shipmentDetails);
        } else {
            if (shipmentDetails.getConsolidationList() == null)
                shipmentDetails.setConsolidationList(new HashSet<>());
            if (shipmentDetails.getContainersList() == null)
                shipmentDetails.setContainersList(new HashSet<>());
        }
        try {
            onSave(shipmentDetails, errors, oldShipment, fromV1Sync);
        } catch (Exception e) {
            String errorMessage = e.getMessage();
            if (e.getClass().equals(ConstraintViolationException.class))
                errorMessage = getConstrainViolationErrorMessage(e);
            throw new RunnerException(errorMessage);
        }
        return shipmentDetails;
    }

    private ShipmentDetails getAndProcessOldEntity(ShipmentDetails shipmentDetails) {
        ShipmentDetails oldShipment;
        long id = shipmentDetails.getId();
        Optional<ShipmentDetails> oldEntity = findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Container is null for Id {}", shipmentDetails.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        if (shipmentDetails.getContainersList() == null) {
            shipmentDetails.setContainersList(oldEntity.get().getContainersList());
        }
        if (shipmentDetails.getConsolidationList() == null ||
                (!CollectionUtils.isEmpty(shipmentDetails.getConsolidationList()) && !CollectionUtils.isEmpty(oldEntity.get().getConsolidationList()) && (Objects.equals(oldEntity.get().getConsolidationList().iterator().next().getId(), shipmentDetails.getConsolidationList().iterator().next().getId())))) {
            shipmentDetails.setConsolidationList(oldEntity.get().getConsolidationList());
        }
        oldShipment = oldEntity.get();
        shipmentDetails.setCreatedBy(oldShipment.getCreatedBy());
        return oldShipment;
    }

    public List<ShipmentDetails> saveAll(List<ShipmentDetails> shipments) throws RunnerException {
        List<ShipmentDetails> res = new ArrayList<>();
        for (ShipmentDetails req : shipments) {
            req = save(req, false);
            res.add(req);
        }
        return res;
    }

    @Override
    public ShipmentDetails update(ShipmentDetails shipmentDetails, boolean fromV1Sync) {
        validateLockStatus(shipmentDetails);
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(shipmentDetails), Constants.SHIPMENT, LifecycleHooks.ON_CREATE, false);
        ShipmentDetails oldShipment = null;
        if (shipmentDetails.getId() != null) {
            long id = shipmentDetails.getId();
            Optional<ShipmentDetails> oldEntity = findById(id);
            if (!oldEntity.isPresent()) {
                log.debug("Shipment is null for Id {}", shipmentDetails.getId());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            if (shipmentDetails.getContainersList() == null) {
                shipmentDetails.setContainersList(oldEntity.get().getContainersList());
            }
            if (shipmentDetails.getConsolidationList() == null ||
                    (!CollectionUtils.isEmpty(shipmentDetails.getConsolidationList()) && !CollectionUtils.isEmpty(oldEntity.get().getConsolidationList()) && (Objects.equals(oldEntity.get().getConsolidationList().iterator().next().getId(), shipmentDetails.getConsolidationList().iterator().next().getId())))) {
                shipmentDetails.setConsolidationList(oldEntity.get().getConsolidationList());
            }
            oldShipment = oldEntity.get();
            shipmentDetails.setCreatedBy(oldShipment.getCreatedBy());
            if (UserContext.getUser() != null) {
                shipmentDetails.setUpdatedBy(UserContext.getUser().getUsername());
            }
            shipmentDetails.setUpdatedAt(LocalDateTime.now());
        }
        onSave(shipmentDetails, errors, oldShipment, fromV1Sync);
        return shipmentDetails;
    }

    private void onSave(ShipmentDetails shipmentDetails, Set<String> errors, ShipmentDetails oldShipment, boolean fromV1Sync) {
        setHouseBill(shipmentDetails, oldShipment);
        errors.addAll(applyShipmentValidations(shipmentDetails, fromV1Sync));
        if (!errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        validateCarrierDetails(shipmentDetails);

        if (!fromV1Sync && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
            validateMawb(shipmentDetails, oldShipment);
        }


        validateIataCode(shipmentDetails);
        long start = System.currentTimeMillis();
        shipmentDetails = shipmentRepository.save(shipmentDetails);
        log.info("{} | Time taken to update shipment query: {} ms", LoggerHelper.getRequestIdFromMDC(), System.currentTimeMillis() - start);
        if (!fromV1Sync && shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT) && shipmentDetails.getMasterBill() != null && !shipmentDetails.getDirection().equals(Constants.IMP)) {
            setMawbStock(shipmentDetails);
        }


    }

    private void validateMawb(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment) {
        if (!Strings.isNullOrEmpty(shipmentDetails.getMasterBill()) && Boolean.FALSE.equals(isMAWBNumberValid(shipmentDetails.getMasterBill())))
            throw new ValidationException("Please enter a valid MAWB number.");
        if ((shipmentDetails.getJobType() != null && shipmentDetails.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)) || (oldShipment != null && oldShipment.getJobType() != null && oldShipment.getJobType().equals(Constants.SHIPMENT_TYPE_DRT)))
            directShipmentMAWBCheck(shipmentDetails, oldShipment != null ? oldShipment.getMasterBill() : null, oldShipment != null ? oldShipment.getJobType() : null);
    }

    private void validateCarrierDetails(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getTransportMode() != null && shipmentDetails.getCarrierDetails() != null) {
            LocalDateTime eta = shipmentDetails.getCarrierDetails().getEta();
            LocalDateTime etd = shipmentDetails.getCarrierDetails().getEtd();
            if (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                //for air shipment, ETA can be less than ETD
                if (isEtaBeforeEtd(eta, etd)) {
                    Duration duration = Duration.between(eta, etd);
                    if (Math.abs(duration.toHours()) > 24) {
                        throw new ValidationException("Difference between ETA and ETD should not be more than 24 hours");
                    }
                }
            } else {
                //for other transport modes other than AIR, ETA cannot be less than ETD
                if (isEtaBeforeEtd(eta, etd)) {
                    throw new ValidationException("ETA should not be less than ETD");
                }
            }
        }
    }

    private boolean isEtaBeforeEtd(LocalDateTime eta, LocalDateTime etd) {
        return eta != null && etd != null && eta.isBefore(etd);
    }

    private void setHouseBill(ShipmentDetails shipmentDetails, ShipmentDetails oldShipment) {
        if (!StringUtility.isEmpty(shipmentDetails.getHouseBill()) && (oldShipment != null && !Objects.equals(oldShipment.getStatus(), shipmentDetails.getStatus())) &&
                Objects.equals(shipmentDetails.getStatus(), ShipmentStatus.Cancelled.getValue())) {
            ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();
            if (tenantSettings != null) {
                String suffix = tenantSettings.getCancelledBLSuffix();
                if (suffix != null) {
                    String newHouseBill = shipmentDetails.getHouseBill() + suffix;
                    shipmentDetails.setHouseBill(newHouseBill);
                }
            }
        }
    }

    @Override
    public Page<ShipmentDetails> findAll(Specification<ShipmentDetails> spec, Pageable pageable) {
        return shipmentRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ShipmentDetails> findById(Long id) {
        return shipmentRepository.findById(id);
    }

    @Override
    public List<ShipmentDetails> findByShipmentId(String shipmentNumber) {
        return shipmentRepository.findByShipmentId(shipmentNumber);
    }

    @Override
    public void delete(ShipmentDetails shipmentDetails) {
        validateLockStatus(shipmentDetails);
        shipmentRepository.delete(shipmentDetails);
    }

    private void validateLockStatus(ShipmentDetails shipmentDetails) throws ValidationException {
        if (shipmentDetails.getIsLocked() != null && shipmentDetails.getIsLocked()) {
            throw new ValidationException(ShipmentConstants.SHIPMENT_LOCKED);
        }
    }

    @Override
    public List<ShipmentDetails> findByGuids(List<UUID> guids) {
        return shipmentRepository.findAllByGuids(guids);
    }

    @Override
    public Optional<ShipmentDetails> findByGuid(UUID id) {
        return shipmentRepository.findByGuid(id);
    }

    @Override
    public List<ShipmentDetails> findByHouseBill(String hbl, Integer tenantId) {
        return shipmentRepository.findByHouseBill(hbl, tenantId);
    }

    @Override
    public List<ShipmentDetails> findByBookingReference(String ref, Integer tenantId) {
        return shipmentRepository.findByBookingReference(ref, tenantId);
    }

    @Override
    public List<CustomerBookingProjection> findCustomerBookingProByShipmentIdIn(List<Long> shipmentIds) {
        return shipmentRepository.findCustomerBookingProByShipmentIdIn(shipmentIds);
    }

    @Override
    public Long findMaxId() {
        return shipmentRepository.findMaxId();
    }

    private boolean checkForNonAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if (!Constants.TRANSPORT_MODE_AIR.equals(request.getTransportMode()))
            return true;
        return !Boolean.TRUE.equals(shipmentSettingsDetails.getAirDGFlag());
    }

    private boolean checkForDGShipmentAndAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if (checkForNonAirDGFlag(request, shipmentSettingsDetails))
            return false;
        return Boolean.TRUE.equals(request.getContainsHazardous());
    }

    private boolean checkForNonDGShipmentAndAirDGFlag(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails) {
        if (checkForNonAirDGFlag(request, shipmentSettingsDetails))
            return false;
        return !Boolean.TRUE.equals(request.getContainsHazardous());
    }

    public Set<String> applyShipmentValidations(ShipmentDetails request, boolean fromV1Sync) {
        Set<String> errors = new LinkedHashSet<>();

        if (Boolean.TRUE.equals(request.getContainsHazardous()) &&
                Constants.TRANSPORT_MODE_SEA.equals(request.getTransportMode()) &&
                Constants.SHIPMENT_TYPE_LCL.equals(request.getShipmentType()) &&
                !Constants.CONSOLIDATION_TYPE_AGT.equals(request.getJobType()) &&
                !Constants.CONSOLIDATION_TYPE_CLD.equals(request.getJobType())) {
            errors.add("For Ocean DG shipments LCL Cargo Type, we can have only AGT and Co Load Master");
        }
        if (request.getConsolidationList() != null && request.getConsolidationList().size() > 1) {
            errors.add("Multiple consolidations are attached to the shipment, please verify.");
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (Boolean.TRUE.equals(countryAirCargoSecurity)) {
            addCargotSecurityValidationErrors(request, fromV1Sync, errors);
        } else {
            addNonDgValidationErrors(request, fromV1Sync, shipmentSettingsDetails, errors);
        }

        // Routings leg no can not be repeated
        addRoutingValidationsErrors(request, errors);

        // Origin and Destinations are mandatory
        addOriginDestinationValidationsError(request, errors);

        // POL and POD are mandatory for Sea and Air
        addPolPodValidationsErrors(request, errors);

        // Container Number can not be repeated
        addContainerNumberValidationsErrors(request, errors);

        // Duplicate party types not allowed
        addPartyTypeValidationErrors(request, errors);

        // Shipment must be attached to consolidation with same master bill
        addMasterBillValidationErrors(request, errors);

        // Shipment restricted unlocations validation
        addUnLocationValidationErrors(request, shipmentSettingsDetails, errors);

        // BL# and Reference No can not be repeated
        addBlValidationErrors(request, errors);
        addBookingReferenceValidationErrors(request, errors);

        return errors;
    }

    private void addCargotSecurityValidationErrors(ShipmentDetails request, boolean fromV1Sync, Set<String> errors) {
        if (!fromV1Sync && !CommonUtils.checkAirSecurityForShipment(request)) {
            errors.add("You don't have Air Security permission to create or update AIR EXP Shipment.");
        }
        // Non dg Shipments can not have dg packs
        if (!Boolean.TRUE.equals(request.getContainsHazardous()) && checkContainsDGPackage(request)) {
            errors.add("The shipment contains DG package. Marking the shipment as non DG is not allowed");
        }
    }

    private void addNonDgValidationErrors(ShipmentDetails request, boolean fromV1Sync, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        // Non dg Shipments can not have dg packs
        if (checkForNonDGShipmentAndAirDGFlag(request, shipmentSettingsDetails) && checkContainsDGPackage(request)) {
            errors.add("The shipment contains DG package. Marking the shipment as non DG is not allowed");
        }

        // Non dg user cannot save dg shipment
        if (!fromV1Sync && checkForDGShipmentAndAirDGFlag(request, shipmentSettingsDetails) && !UserContext.isAirDgUser())
            errors.add("You don't have permission to update DG Shipment");
    }

    private void addRoutingValidationsErrors(ShipmentDetails request, Set<String> errors) {
        if (request.getRoutingsList() != null && !request.getRoutingsList().isEmpty()) {
            HashSet<Long> hashSet = new HashSet<>();
            for (Routings routingsRequest : request.getRoutingsList()) {
                if (routingsRequest.getLeg() != null) {
                    if (hashSet.contains(routingsRequest.getLeg())) {
                        errors.add("Leg No in routings cannot be same for two different legs");
                        break;
                    } else
                        hashSet.add(routingsRequest.getLeg());
                }
            }
        }
    }

    private void addOriginDestinationValidationsError(ShipmentDetails request, Set<String> errors) {
        if (request.getCarrierDetails() == null || isStringNullOrEmpty(request.getCarrierDetails().getOrigin()) || isStringNullOrEmpty(request.getCarrierDetails().getDestination()))
            errors.add("Origin and Destination fields are mandatory.");
        if (request.getCarrierDetails() != null && Objects.equals(request.getCarrierDetails().getOrigin(), request.getCarrierDetails().getDestination())) {
            errors.add("Origin and Destination fields cannot be same.");
        }
        if (request.getCarrierDetails() != null && Objects.equals(request.getCarrierDetails().getOrigin(), request.getCarrierDetails().getDestinationPort())) {
            errors.add("Origin and POD fields cannot be same.");
        }
    }

    private void addPolPodValidationsErrors(ShipmentDetails request, Set<String> errors) {
        if ((Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) &&
                (request.getCarrierDetails() == null || isStringNullOrEmpty(request.getCarrierDetails().getOriginPort()) || isStringNullOrEmpty(request.getCarrierDetails().getDestinationPort())))
            errors.add("POL and POD fields are mandatory.");
        if (request.getCarrierDetails() != null && Objects.equals(request.getCarrierDetails().getOriginPort(), request.getCarrierDetails().getDestinationPort())) {
            errors.add("POL and POD fields cannot be same.");
        }
        if (request.getCarrierDetails() != null && Objects.equals(request.getCarrierDetails().getOriginPort(), request.getCarrierDetails().getDestination())) {
            errors.add("POL and Destination fields cannot be same.");
        }
    }

    private void addContainerNumberValidationsErrors(ShipmentDetails request, Set<String> errors) {
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

    private void addPartyTypeValidationErrors(ShipmentDetails request, Set<String> errors) {
        if (request.getShipmentAddresses() != null && !request.getShipmentAddresses().isEmpty()) {
            HashSet<String> partyTypes = new HashSet<>();
            HashSet<String> duplicatePartyTypes = new HashSet<>();
            for (Parties item : request.getShipmentAddresses()) {
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

    private void addMasterBillValidationErrors(ShipmentDetails request, Set<String> errors) {
        if (!isStringNullOrEmpty(request.getMasterBill())) {
            var consoleList = consolidationDetailsDao.findByBol(request.getMasterBill());
            if (!consoleList.isEmpty()) {
                ConsolidationDetails console = consoleList.get(0);

                if ((request.getConsolidationList() != null && !request.getConsolidationList().stream().map(ConsolidationDetails::getId).toList().contains(console.getId())) ||
                        (request.getConsolidationList() == null &&
                                (request.getId() == null ||
                                        (console.getShipmentsList() != null && !console.getShipmentsList().stream().map(BaseEntity::getId).toList().contains(request.getId()))))) {
                    String message = "%s %s is linked to consolidation %s. Please attach the shipment to that consolidation.";
                    String masterBillType = request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) ? Constants.MAWB : Constants.MBL;
                    errors.add(String.format(message, masterBillType, request.getMasterBill(), console.getConsolidationNumber()));
                }
            }
        }
    }

    private void addUnLocationValidationErrors(ShipmentDetails request, ShipmentSettingsDetails shipmentSettingsDetails, Set<String> errors) {
        if (Boolean.TRUE.equals(shipmentSettingsDetails.getRestrictedLocationsEnabled()) && request.getCarrierDetails() != null) {
            String unLoc = null;
            if (request.getDirection().equals(Constants.DIRECTION_EXP)) {
                unLoc = request.getCarrierDetails().getOriginPort();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Loading Port is not allowed or invalid");
                }
            } else if (request.getDirection().equals(Constants.IMP)) {
                unLoc = request.getCarrierDetails().getDestinationPort();
                if (shipmentSettingsDetails.getRestrictedLocations() == null || !shipmentSettingsDetails.getRestrictedLocations().contains(unLoc)) {
                    errors.add("Value entered for Discharge Port is not allowed or invalid");
                }
            }
        }
    }

    private void addBlValidationErrors(ShipmentDetails request, Set<String> errors) {
        if (!isStringNullOrEmpty(request.getHouseBill())) {
            List<ShipmentDetails> shipmentDetails = findByHouseBill(request.getHouseBill(), TenantContext.getCurrentTenant());
            if (shipmentDetails != null && !shipmentDetails.isEmpty() && (request.getId() == null || shipmentDetails.get(0).getId().longValue() != request.getId().longValue())) {
                if (Objects.equals(request.getStatus(), ShipmentStatus.Cancelled.getValue()))
                    errors.add("Canceled HBL is already available in the application. Please remove/ modify the HBL number to proceed further");
                else
                    errors.add("Shipment with BL# " + request.getHouseBill() + " already exists.");
            }
        }
    }

    private void addBookingReferenceValidationErrors(ShipmentDetails request, Set<String> errors) {
        if (!isStringNullOrEmpty(request.getBookingReference())) {
            List<ShipmentDetails> shipmentDetails = findByBookingReference(request.getBookingReference(), TenantContext.getCurrentTenant());
            if (!shipmentDetails.isEmpty() && (request.getId() == null || shipmentDetails.get(0).getId().longValue() != request.getId().longValue())) {
                errors.add("Shipment with ReferenceNo " + request.getBookingReference() + " already exists.");
            }
        }
    }

    private void setMawbStock(ShipmentDetails shipmentDetails) {
        List<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findByMawbNumber(shipmentDetails.getMasterBill());
        if (mawbStocksLinks != null && !mawbStocksLinks.isEmpty()) {
            MawbStocksLink res = mawbStocksLinks.get(0);
            if (!Objects.isNull(res.getStatus()) && !res.getStatus().equals(CONSUMED)) {
                res.setEntityId(shipmentDetails.getId());
                res.setEntityType(Constants.SHIPMENT);
                res.setShipConsNumber(shipmentDetails.getShipmentId());
                res.setStatus(CONSUMED);
                mawbStocksLinkDao.save(res);
                setAvaliableCount(res.getParentId());
            }
        }
    }

    private void setAvaliableCount(Long parentId) {
        Optional<MawbStocks> mawbStocks = mawbStocksDao.findById(parentId);
        if (!mawbStocks.isEmpty()) {
            MawbStocks res = mawbStocks.get();
            res.setAvailableCount(String.valueOf(Integer.parseInt(res.getAvailableCount() == null ? "1" : res.getAvailableCount()) - 1));
            res.setNextMawbNumber(assignNextMawbNumber(parentId));
            mawbStocksDao.save(res);
        }
    }

    private String assignNextMawbNumber(Long parentId) {
        ListCommonRequest listCommonRequest;
        listCommonRequest = CommonUtils.andCriteria("parentId", parentId, "=", null);
        CommonUtils.andCriteria("status", UNUSED, "=", listCommonRequest);
        listCommonRequest.setSortRequest(SortRequest.builder()
                .fieldName("seqNumber")
                .order("DESC")
                .build());
        Pair<Specification<MawbStocksLink>, Pageable> pair = fetchData(listCommonRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinks = mawbStocksLinkDao.findAll(pair.getLeft(), pair.getRight());
        if (!mawbStocksLinks.isEmpty()) {
            return mawbStocksLinks.get().toList().get(0).getMawbNumber();
        }
        return null;
    }

    private void directShipmentMAWBCheck(ShipmentDetails shipmentRequest, String oldMasterBill, String oldJobType) {

        if (StringUtility.isEmpty(shipmentRequest.getMasterBill())) {
            if (!shipmentRequest.getDirection().equals("IMP")) {
                mawbStocksLinkDao.deLinkExistingMawbStockLink(oldMasterBill);
            }
            return;
        }
        if (!shipmentRequest.getDirection().equals("IMP") &&
                (!Objects.equals(shipmentRequest.getMasterBill(), oldMasterBill) ||
                        (!StringUtility.isEmpty(oldMasterBill) && Constants.SHIPMENT_TYPE_DRT.equals(oldJobType) && !Constants.SHIPMENT_TYPE_DRT.equals(shipmentRequest.getJobType())))) {
            mawbStocksLinkDao.deLinkExistingMawbStockLink(oldMasterBill);
        }

        CarrierResponse correspondingCarrier = getCorrespondingCarrier(shipmentRequest, oldMasterBill);

        if (shipmentRequest.getDirection().equals("IMP")) {
            return;
        }

        ListCommonRequest listMawbRequest = constructListCommonRequest("mawbNumber", shipmentRequest.getMasterBill(), "=");
        Pair<Specification<MawbStocksLink>, Pageable> mawbStocksLinkPair = fetchData(listMawbRequest, MawbStocksLink.class);
        Page<MawbStocksLink> mawbStocksLinkPage = mawbStocksLinkDao.findAll(mawbStocksLinkPair.getLeft(), mawbStocksLinkPair.getRight());

        validateAndHandleMAWB(shipmentRequest, mawbStocksLinkPage, correspondingCarrier);
    }

    private void validateAndHandleMAWB(ShipmentDetails shipmentRequest, Page<MawbStocksLink> mawbStocksLinkPage, CarrierResponse correspondingCarrier) {
        boolean isMAWBNumberExist = false;
        MawbStocksLink mawbStocksLink = null;

        if (!mawbStocksLinkPage.isEmpty() && mawbStocksLinkPage.getTotalElements() > 0) {
            isMAWBNumberExist = true;
            mawbStocksLink = mawbStocksLinkPage.getContent().get(0);
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        if (isMAWBNumberExist) {
            if (mawbStocksLink.getStatus().equals(CONSUMED) && !Objects.equals(mawbStocksLink.getEntityId(), shipmentRequest.getId())) // If MasterBill number is already Consumed.
                throw new ValidationException("The MAWB number entered is already consumed. Please enter another MAWB number.");
            else if (Boolean.TRUE.equals(shipmentSettingsDetails.getIsRunnerV3Enabled()) && !Objects.equals(mawbStocksLink.getEntityId(), shipmentRequest.getId())) {
                var mawbStock = mawbStocksDao.findById(mawbStocksLink.getParentId());
                if(mawbStock.isEmpty()){
                    throw new DataRetrievalFailureException("No stock entry found for given mawb number stock link");
                }
                if(validatedBorrowedFrom(shipmentRequest, mawbStock.get())) {
                    throw new ValidationException("Entered MAWB is linked with Borrowed from Party, please amend the Partner details to None.");
                }
                if(StringUtility.isNotEmpty(mawbStock.get().getBorrowedFrom())) {
                    shipmentRequest.setIsBorrowed(true);
                    Parties borrowedParty = v1ServiceUtil.getOrganizationDataFromV1(mawbStock.get().getBorrowedFrom());
                    shipmentRequest.getAdditionalDetails().setBorrowedFrom(borrowedParty);
                }
            }
        } else {
            createNewMAWBEntry(shipmentRequest, correspondingCarrier != null ? correspondingCarrier.getItemValue() : shipmentRequest.getCarrierDetails().getShippingLine());
        }
    }

    private boolean validatedBorrowedFrom(ShipmentDetails shipmentRequest, MawbStocks mawbStocks) {
        if (StringUtility.isEmpty(mawbStocks.getBorrowedFrom())) return false;
        if(shipmentRequest.getAdditionalDetails() == null) return false;
        if (Objects.isNull(shipmentRequest.getAdditionalDetails().getBorrowedFrom())) return false;
        if (StringUtility.isEmpty(shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgCode())) return false;
        return !Objects.equals(shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgCode(), mawbStocks.getBorrowedFrom());
    }

    private CarrierResponse getCorrespondingCarrier(ShipmentDetails shipmentRequest, String oldMasterBill) {
        CarrierResponse correspondingCarrier = null;
        if (shipmentRequest.getCarrierDetails() == null || StringUtility.isEmpty(shipmentRequest.getCarrierDetails().getShippingLine()) ||
                !Objects.equals(shipmentRequest.getMasterBill(), oldMasterBill)) {
            String mawbAirlineCode = shipmentRequest.getMasterBill().substring(0, 3);

            V1DataResponse v1DataResponse = fetchCarrierDetailsFromV1(mawbAirlineCode, shipmentRequest.getJobType());
            List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
            if (carrierDetails == null || carrierDetails.isEmpty())
                throw new ValidationException("Airline for the entered MAWB Number doesn't exist in Carrier Master");

            correspondingCarrier = carrierDetails.get(0);

            if (shipmentRequest.getCarrierDetails() == null || StringUtility.isEmpty(shipmentRequest.getCarrierDetails().getShippingLine())) {
                if (shipmentRequest.getCarrierDetails() == null)
                    shipmentRequest.setCarrierDetails(new CarrierDetails());

                shipmentRequest.getCarrierDetails().setShippingLine(correspondingCarrier.getItemValue());
            }
        }
        return correspondingCarrier;
    }

    private void createNewMAWBEntry(ShipmentDetails shipmentRequest, String shippingLine) {
        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setAirLinePrefix(shippingLine);
        mawbStocks.setCount("1");
        mawbStocks.setAvailableCount("1");
        mawbStocks.setStartNumber(Long.valueOf(shipmentRequest.getMasterBill().substring(4, 10)));
        mawbStocks.setFrom(shipmentRequest.getMasterBill());
        mawbStocks.setTo(shipmentRequest.getMasterBill());
        mawbStocks.setMawbNumber(shipmentRequest.getMasterBill());
        mawbStocks.setStatus(UNUSED);
        if (shipmentRequest.getAdditionalDetails().getBorrowedFrom() != null) {
            mawbStocks.setBorrowedFrom(shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgCode());
            if (shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgData() != null && shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgData().containsKey("FullName")) {
                String name = (String) shipmentRequest.getAdditionalDetails().getBorrowedFrom().getOrgData().get("FullName");
                mawbStocks.setBorrowedFromFullName(name);
            }
        }
        mawbStocks.setHomePort(shipmentRequest.getCarrierDetails().getOriginPort());
        mawbStocks = mawbStocksDao.save(mawbStocks);

        if (mawbStocks.getId() != null) {
            var entryForMawbStocksLinkRow = new MawbStocksLink();
            entryForMawbStocksLinkRow.setParentId(mawbStocks.getId());
            entryForMawbStocksLinkRow.setSeqNumber(shipmentRequest.getMasterBill().substring(4, 10));
            entryForMawbStocksLinkRow.setMawbNumber(shipmentRequest.getMasterBill());
            entryForMawbStocksLinkRow.setStatus(UNUSED);
            mawbStocksLinkDao.save(entryForMawbStocksLinkRow);
        }
    }

    private Boolean isMAWBNumberValid(String masterBill) {
        boolean mAWBNumberValidity = true;
        if (masterBill.length() == 12) {
            String mawbSeqNum = masterBill.substring(4, 11);
            String checkDigit = masterBill.substring(11, 12);
            if (areAllCharactersDigits(masterBill, 4, 12)) { // masterBill.substring(4, 12).matches("\\d+")
                long imawbSeqNum = Long.parseLong(mawbSeqNum);
                long icheckDigit = Long.parseLong(checkDigit);
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

    private V1DataResponse fetchCarrierDetailsFromV1(String mawbAirlineCode, String type) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("AirlineCode"), "=", mawbAirlineCode));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("HasAirPort"), "=", 1));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        carrierListObject.setType(type);
        return v1Service.fetchCarrierMasterData(carrierListObject, false);
    }

    private void validateIataCode(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getTransportMode() != null && shipmentDetails.getCarrierDetails() != null && StringUtility.isNotEmpty(shipmentDetails.getCarrierDetails().getShippingLine())
                && shipmentDetails.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
            V1DataResponse v1DataResponse = fetchCarrier(shipmentDetails.getCarrierDetails().getShippingLine());
            List<CarrierResponse> carrierDetails = jsonHelper.convertValueToList(v1DataResponse.entities, CarrierResponse.class);
            if (carrierDetails == null || carrierDetails.isEmpty() || StringUtility.isEmpty(carrierDetails.get(0).iATACode))
                throw new ValidationException("Please add the IATA code in the Carrier Master for " + shipmentDetails.getCarrierDetails().getShippingLine());
        }
    }

    private V1DataResponse fetchCarrier(String shippingLine) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>(List.of(List.of("ItemValue"), "=", shippingLine));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        return v1Service.fetchCarrierMasterData(carrierListObject, true);
    }

    @Transactional
    public void saveJobStatus(Long id, String jobStatus) {
        shipmentRepository.saveJobStatus(id, jobStatus);
    }

    @Transactional
    public void saveStatus(Long id, Integer status) {
        shipmentRepository.saveStatus(id, status);
    }

    @Transactional
    public void saveCreatedDateAndUser(Long id, String createdBy, LocalDateTime createdDate) {
        shipmentRepository.saveCreatedDateAndUser(id, createdBy, createdDate);
    }

    @Override
    public List<ShipmentDetails> getShipmentNumberFromId(List<Long> shipmentIds) {
        return shipmentRepository.getShipmentNumberFromId(shipmentIds);
    }

    @Override
    @Transactional
    public void saveEntityTransfer(Long id, Boolean entityTransfer) {
        shipmentRepository.saveEntityTransfer(id, entityTransfer);
    }

    @Override
    @Transactional
    public List<ShipmentDetails> findShipmentsByGuids(Set<UUID> guids) {
        return shipmentRepository.findShipmentsByGuids(guids);
    }

    @Override
    @Transactional
    public List<ShipmentDetails> findShipmentsBySourceGuids(Set<UUID> sourceGuid) {
        return shipmentRepository.findShipmentsBySourceGuids(sourceGuid);
    }

    @Override
    public List<ShipmentDetails> findShipmentBySourceGuidAndTenantId(UUID sourceGuid, Integer tenantId) {
        return shipmentRepository.findShipmentBySourceGuidAndTenantId(sourceGuid, tenantId);
    }

    @Override
    @Transactional
    public List<ShipmentDetails> findShipmentsByIds(Set<Long> ids) {
        return shipmentRepository.findShipmentsByIds(ids);
    }

    @Override
    @Transactional
    public Optional<ShipmentDetails> findShipmentByIdWithQuery(Long id) {
        return shipmentRepository.findShipmentByIdWithQuery(id);
    }

    @Override
    @Transactional
    public void entityDetach(List<ShipmentDetails> shipmentDetails) {
        for (ShipmentDetails shipmentDetails1 : shipmentDetails) {
            entityManager.detach(shipmentDetails1);
        }
    }

    @Override
    public List<ShipmentDetails> findBySourceGuid(UUID guid) {
        return shipmentRepository.findBySourceGuid(guid);
    }

    @Override
    public Page<Long> getIdWithPendingActions(ShipmentRequestedType shipmentRequestedType, Pageable pageable) {
        return shipmentRepository.getIdWithPendingActions(shipmentRequestedType, pageable);
    }

    @Override
    public List<ShipmentDetailsProjection> findByHblNumberAndExcludeShipmentId(String hblNumber, String shipmentId) {
        return shipmentRepository.findByHblNumberAndExcludeShipmentId(hblNumber, shipmentId);
    }

    @Override
    public List<ShipmentDetailsProjection> findShipmentDetailsByAttachedContainerIds(List<Long> containerIds) {
        return shipmentRepository.findShipmentDetailsByAttachedContainerIds(containerIds);
    }

    @Override
    public Page<ShipmentDetails> findAllWithoutTenantFilter(Specification<ShipmentDetails> spec, Pageable pageable) {
        return shipmentRepository.findAllWithoutTenantFilter(spec, pageable);
    }

    @Override
    public ShipmentDetails saveWithoutValidation(ShipmentDetails shipmentDetails) {
        return shipmentRepository.save(shipmentDetails);
    }

    @Override
    public void updateAdditionalDetailsByShipmentId(Long id, boolean emptyContainerReturned) {
        shipmentRepository.updateAdditionalDetailsByShipmentId(id, emptyContainerReturned);
    }

    @Override
    public List<ShipmentDetails> findByShipmentIdInAndContainsHazardous(List<Long> shipmentIdList,
                                                                        boolean containsHazardous) {
        return shipmentRepository.findByShipmentIdInAndContainsHazardous(shipmentIdList, containsHazardous);
    }

    @Override
    public List<ShipmentDetails> findByShipmentIdIn(List<String> shipmentIds) {
        return shipmentRepository.findByShipmentIdIn(shipmentIds);
    }

    @Override
    @Transactional
    public void saveIsTransferredToReceivingBranch(Long id, Boolean entityTransferred) {
        shipmentRepository.saveIsTransferredToReceivingBranch(id, entityTransferred);
    }

    @Override
    @Transactional
    public void updateIsAcceptedTriangulationPartner(Long shipmentId, Long triangulationPartner, Boolean isAccepted) {
        shipmentRepository.updateIsAcceptedTriangulationPartner(shipmentId, triangulationPartner, isAccepted);
    }

    @Override
    @Transactional
    public void updateFCRNo(Long id) {
        shipmentRepository.updateFCRNo(id);
    }

    @Override
    @Transactional
    public Optional<ShipmentDetails> findShipmentByGuidWithQuery(UUID guid) {
        return shipmentRepository.findShipmentByGuidWithQuery(guid);
    }

    @Override
    @Transactional
    public int updateShipmentsBookingNumber(List<UUID> guids, String bookingNumber) {
        return shipmentRepository.updateShipmentsBookingNumber(guids, bookingNumber);
    }

    @Override
    public Integer findReceivingByGuid(UUID guid) {
        return shipmentRepository.findReceivingByGuid(guid);
    }

    @Override
    public void updateCargoDetailsInShipment(Long shipmentId, Integer noOfPacks, String packsUnit, BigDecimal volume, String volumeUnit, BigDecimal weight, String weightUnit, BigDecimal volumetricWeight, String volumetricWeightUnit, BigDecimal chargable, String chargeableUnit) {
        shipmentRepository.updateCargoDetailsInShipment(shipmentId, noOfPacks, packsUnit, volume, volumeUnit, weight, weightUnit, volumetricWeight, volumetricWeightUnit, chargable, chargeableUnit);
    }

    @Override
    public void updateShipmentDetailsFromPacks(Long shipmentId, DateBehaviorType dateType, LocalDateTime shipmentGateInDate, ShipmentPackStatus shipmentPackStatus) {
        shipmentRepository.updateShipmentDetailsFromPacks(shipmentId, dateType, shipmentGateInDate, shipmentPackStatus);
    }

    private boolean checkContainsDGPackage(ShipmentDetails request) {
        if (CommonUtils.listIsNullOrEmpty(request.getPackingList()))
            return false;
        for (Packing packing : request.getPackingList()) {
            if (Boolean.TRUE.equals(packing.getHazardous())) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void setShipmentIdsToContainer(List<Long> shipmentIds, Long containerId) {
        shipmentRepository.setShipmentIdsToContainer(shipmentIds, containerId);
    }

    @Override
    public void updateSailingScheduleRelatedInfo(ShipmentSailingScheduleRequest request, Long shipmentId) {
        shipmentRepository.updateSailingScheduleRelatedInfo(shipmentId, request.getTerminalCutoff(), request.getVerifiedGrossMassCutoff(),
                request.getShippingInstructionCutoff(), request.getDgCutoff(), request.getReeferCutoff(),
                request.getEarliestEmptyEquipmentPickUp(), request.getLatestFullEquipmentDeliveredToCarrier(),
                request.getEarliestDropOffFullEquipmentToCarrier());
    }

    @Override
    public void updateSailingScheduleRelatedInfoForAir(ShipmentSailingScheduleRequest request, Long shipmentId) {
        shipmentRepository.updateSailingScheduleRelatedInfoForAir(shipmentId, request.getLatestArrivalTime());
    }

    @Override
    public List<ShipmentDetails> findByIdIn(List<Long> shipmentIds) {
        return shipmentRepository.findAllById(shipmentIds);
    }
}
