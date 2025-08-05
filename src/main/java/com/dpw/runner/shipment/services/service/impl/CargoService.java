package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoChargeableRequest;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoChargeableResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingV3Service;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Service
@Slf4j
public class CargoService implements ICargoService {

    private final IMDMServiceAdapter mdmServiceAdapter;
    private final ICustomerBookingDao customerBookingDao;
    private final IShipmentDao shipmentDao;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final JsonHelper jsonHelper;
    private final IConsolidationService consolidationService;
    private final ICustomerBookingV3Service customerBookingV3Service;

    @Autowired
    public CargoService(IMDMServiceAdapter mdmServiceAdapter,
                        ICustomerBookingDao customerBookingDao,
                        IShipmentDao shipmentDao,
                        IConsolidationDetailsDao consolidationDetailsDao,
                        JsonHelper jsonHelper,
                        IConsolidationService consolidationService,
                        ICustomerBookingV3Service customerBookingV3Service) {
        this.mdmServiceAdapter = mdmServiceAdapter;
        this.customerBookingDao = customerBookingDao;
        this.shipmentDao = shipmentDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.jsonHelper = jsonHelper;
        this.consolidationService = consolidationService;
        this.customerBookingV3Service = customerBookingV3Service;
    }

    @Override
    public CargoDetailsResponse getCargoDetails(CargoDetailsRequest request) throws RunnerException {
        CargoDetailsResponse response = new CargoDetailsResponse();
        String entityType = request.getEntityType();
        Long entityId = Long.valueOf(request.getEntityId());
        List<Containers> containers = fetchContainers(entityType, entityId);
        List<Packing> packings = fetchPackings(entityType, entityId);
        response.setTransportMode(fetchTransportType(entityType, entityId));
        response.setShipmentType(fetchShipmentType(entityType, entityId));
        response.setContainers(null);
        response.setTeuCount(null);
        if (!containers.isEmpty()) {
            Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
            response.setContainers(getTotalContainerCount(containers));
            response.setTeuCount(getTotalTeu(containers, codeTeuMap));
        }
        if (!packings.isEmpty()) {
            calculateCargoDetails(packings, response);
        }
        calculateVW(response);
        response.setIsDifferenceInPackages(calculatePackageDifference(containers, packings) ? Boolean.TRUE : Boolean.FALSE);
        response.setIsDifferenceInCargoWeight(calculateCargoWeightDifference(containers, packings));
        return response;
    }

    private boolean calculatePackageDifference(List<Containers> containersList, List<Packing> packingList) {
        if(containersList.isEmpty() || packingList.isEmpty()) {
            return false;
        }
        Long totalContainerPackages = customerBookingV3Service.getTotalContainerPackages(containersList);
        Long totalPackingSectionPackages = 0L;

        for(Packing pack: packingList) {
            totalPackingSectionPackages += Long.parseLong(pack.getPacks());
        }
        return Math.abs(totalContainerPackages - totalPackingSectionPackages) > 0L;
    }

    private boolean calculateCargoWeightDifference(List<Containers> containersList, List<Packing> packingList) {
        if(containersList.isEmpty() || packingList.isEmpty()) {
            return false;
        }

        BigDecimal totalContainerCargoWeight = customerBookingV3Service.getTotalCargoWeight(containersList);
        BigDecimal totalPackingSectionCargoWeight = BigDecimal.ZERO;

        for(Packing pack: packingList) {
            if(pack.getWeight() != null) {
                totalPackingSectionCargoWeight = totalPackingSectionCargoWeight.add(pack.getWeight());
            }
        }

        return totalContainerCargoWeight.subtract(totalPackingSectionCargoWeight).abs().compareTo(BigDecimal.ZERO) > 0;
    }

    @Override
    public CargoChargeableResponse calculateChargeable(CargoChargeableRequest request) throws RunnerException {
        VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(
                request.getTransportMode(),
                request.getWeightUnit(),
                request.getVolumeUnit(),
                request.getWeight(),
                request.getVolume()
        );

        BigDecimal chargeable = vwOb.getChargeable();
        if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(request.getTransportMode())) {
            chargeable = BigDecimal.valueOf(roundOffAirShipment(chargeable.doubleValue()));
        }

        CargoChargeableResponse response = new CargoChargeableResponse();
        response.setWeight(request.getWeight());
        response.setWeightUnit(request.getWeightUnit());
        response.setChargeable(chargeable);
        response.setChargeableUnit(vwOb.getChargeableUnit());
        response.setVolumetricWeight(vwOb.getVolumeWeight());
        response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        return response;
    }

    private Map<String, BigDecimal> getCodeTeuMapping() throws RunnerException {
        DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
        List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse.getData(), MdmContainerTypeResponse.class);
        return containerTypes.stream().collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
    }

    private int getTotalContainerCount(List<Containers> containers) {
        return containers.stream().mapToInt(c -> c.getContainerCount() != null ? c.getContainerCount().intValue() : 0).sum();
    }

    private BigDecimal getTotalTeu(List<Containers> containers, Map<String, BigDecimal> teuMap) {
        return containers.stream()
                .map(c -> teuMap.getOrDefault(c.getContainerCode(), BigDecimal.ZERO)
                        .multiply(BigDecimal.valueOf(Optional.ofNullable(c.getContainerCount()).orElse(0L))))
                .reduce(BigDecimal.ZERO, BigDecimal::add)
                .setScale(1, RoundingMode.UNNECESSARY);
    }

    private void calculateCargoDetails(List<Packing> packings, CargoDetailsResponse response) throws RunnerException {
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;
        int totalPacks = 0;
        boolean stopWeightCalculation = false;
        Set<String> distinctPackTypes = new HashSet<>();
        boolean isAirTransport = Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(response.getTransportMode());
        response.setWeightUnit(Constants.WEIGHT_UNIT_KG);
        response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        for (Packing packing : packings) {
            totalVolume = addPackingVolume(totalVolume, packing, response.getVolumeUnit());
            if (!isStringNullOrEmpty(packing.getPacks())) {
                totalPacks += Integer.parseInt(packing.getPacks());
            }
            addDistinctPackType(distinctPackTypes, packing);
            if (!stopWeightCalculation) {
                boolean hasWeight = packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit());
                if (isAirTransport && !hasWeight) {
                    stopWeightCalculation = true;
                    continue;
                }
                BigDecimal weight = hasWeight ? new BigDecimal(convertUnit(MASS, packing.getWeight(), packing.getWeightUnit(), response.getWeightUnit()).toString()) : BigDecimal.ZERO;
                totalWeight = totalWeight.add(weight);
            }
        }
        response.setVolume(totalVolume);
        response.setNoOfPacks(totalPacks);
        response.setPacksUnit(getPackUnit(distinctPackTypes));
        response.setWeight(totalWeight);
    }

    private BigDecimal addPackingVolume(BigDecimal totalVolume, Packing packing, String volumeUnit) throws RunnerException {
        if (packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit())) {
            BigDecimal converted = new BigDecimal(
                    convertUnit(VOLUME, packing.getVolume(), packing.getVolumeUnit(), volumeUnit).toString()
            );
            return totalVolume.add(converted);
        }
        return totalVolume;
    }

    private void addDistinctPackType(Set<String> distinctPackTypes, Packing packing) {
        if (!isStringNullOrEmpty(packing.getPacksType())) {
            distinctPackTypes.add(packing.getPacksType());
        }
    }

    private String getPackUnit(Set<String> packTypes) {
        return (packTypes.size() == 1) ? packTypes.iterator().next() : PackingConstants.PKG;
    }

    private void calculateVW(CargoDetailsResponse response) throws RunnerException {
        if (isStringNullOrEmpty(response.getTransportMode()))
            return;
        if (!isStringNullOrEmpty(response.getWeightUnit()) && !isStringNullOrEmpty(response.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(response.getTransportMode(), response.getWeightUnit(), response.getVolumeUnit(), response.getWeight(), response.getVolume());
            response.setChargable(vwOb.getChargeable());
            if (Constants.TRANSPORT_MODE_AIR.equals(response.getTransportMode())) {
                response.setChargable(BigDecimal.valueOf(roundOffAirShipment(response.getChargable().doubleValue())));
            }
            response.setChargeableUnit(vwOb.getChargeableUnit());
            if (Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) && !isStringNullOrEmpty(response.getShipmentType()) && Constants.SHIPMENT_TYPE_LCL.equals(response.getShipmentType())) {
                double volInM3 = convertUnit(Constants.VOLUME, response.getVolume(), response.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, response.getWeight(), response.getWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                response.setChargable(BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3)));
                response.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                vwOb = consolidationService.calculateVolumeWeight(response.getTransportMode(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
            }

            response.setVolumetricWeight(vwOb.getVolumeWeight());
            response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        }
    }

    private double roundOffAirShipment(double charge) {
        return (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) ?
                Math.floor(charge) + 0.5 : Math.ceil(charge);
    }

    private <T> List<T> fetchEntityDataList(String entityType, Long entityId, Function<CustomerBooking, Collection<T>> bookingFn, Function<ShipmentDetails, Collection<T>> shipmentFn, Function<ConsolidationDetails, Collection<T>> consolidationFn) {
        return switch (entityType.toUpperCase()) {
            case "BOOKING" -> customerBookingDao.findById(entityId)
                    .map(bookingFn)
                    .map(list -> new ArrayList<>(list))
                    .orElseGet(ArrayList::new);

            case "SHIPMENT" -> shipmentDao.findById(entityId)
                    .map(shipmentFn)
                    .map(list -> new ArrayList<>(list))
                    .orElseGet(ArrayList::new);

            case "CONSOLIDATION" -> consolidationDetailsDao.findById(entityId)
                    .map(consolidationFn)
                    .map(list -> new ArrayList<>(list))
                    .orElseGet(ArrayList::new);

            default -> {
                log.error("Unknown entityType '{}' in request", entityType);
                yield new ArrayList<>();
            }
        };
    }

    private <T> T fetchSingleEntityData(
            String entityType,
            Long entityId,
            Function<CustomerBooking, T> bookingFn,
            Function<ShipmentDetails, T> shipmentFn,
            Function<ConsolidationDetails, T> consolidationFn
    ) {
        return switch (entityType.toUpperCase()) {
            case BOOKING -> customerBookingDao.findById(entityId)
                    .map(bookingFn)
                    .orElse(null);

            case SHIPMENT -> shipmentDao.findById(entityId)
                    .map(shipmentFn)
                    .orElse(null);

            case CONSOLIDATION -> consolidationDetailsDao.findById(entityId)
                    .map(consolidationFn)
                    .orElse(null);

            default -> {
                log.error("Unknown entityType '{}' in request", entityType);
                yield null;
            }
        };
    }

    private List<Containers> fetchContainers(String entityType, Long entityId) {
        return fetchEntityDataList(
                entityType,
                entityId,
                CustomerBooking::getContainersList,
                ShipmentDetails::getContainersList,
                ConsolidationDetails::getContainersList
        );
    }

    private List<Packing> fetchPackings(String entityType, Long entityId) {
        return fetchEntityDataList(
                entityType,
                entityId,
                CustomerBooking::getPackingList,
                ShipmentDetails::getPackingList,
                ConsolidationDetails::getPackingList
        );
    }

    private String fetchTransportType(String entityType, Long entityId) {
        return fetchSingleEntityData(
                entityType,
                entityId,
                CustomerBooking::getTransportType,
                ShipmentDetails::getTransportMode,
                ConsolidationDetails::getTransportMode
        );
    }

    private String fetchShipmentType(String entityType, Long entityId) {
        return fetchSingleEntityData(
                entityType,
                entityId,
                CustomerBooking::getCargoType,
                ShipmentDetails::getShipmentType,
                ConsolidationDetails::getContainerCategory
        );
    }
}