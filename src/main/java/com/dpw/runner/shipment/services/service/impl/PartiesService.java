package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.Conditions;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class PartiesService {

  private final V1ServiceUtil v1ServiceUtil;
  private final ModelMapper modelMapper;

  public PartiesService(V1ServiceUtil v1ServiceUtil, ModelMapper modelMapper) {
    this.v1ServiceUtil = v1ServiceUtil;
    this.modelMapper = modelMapper;
    modelMapper.getConfiguration().setPropertyCondition(Conditions.isNotNull());
  }

  public void partialUpdateParties(ShipmentPatchRequest shipmentPatchRequest,
      ShipmentDetails entity) {
    Parties patchClient = Optional.ofNullable(shipmentPatchRequest.getClient())
        .map(client -> modelMapper.map(client, Parties.class))
        .orElse(null);
    Parties patchConsignee = Optional.ofNullable(shipmentPatchRequest.getConsignee())
        .map(consignee -> modelMapper.map(consignee, Parties.class)).orElse(null);
    Parties patchConsignor = Optional.ofNullable(shipmentPatchRequest.getConsigner())
        .map(consigner -> modelMapper.map(consigner, Parties.class)).orElse(null);
    Parties patchNotifyParty = Optional.ofNullable(shipmentPatchRequest.getNotifyParty())
        .map(notifyParty -> modelMapper.map(notifyParty, Parties.class)).orElse(null);

    List<Parties> parties = Arrays.asList(
        patchClient,
        patchConsignee,
        patchConsignor,
        patchNotifyParty
    );

    OrgAddressResponse orgAddressResponseForRequests = v1ServiceUtil.fetchOrgInfoFromV1(parties);

    Map<String, Map<String, Object>> addressMapForRequests = orgAddressResponseForRequests.getAddresses();
    Map<String, Map<String, Object>> orgMapForRequests = orgAddressResponseForRequests.getOrganizations();

    setAllPartiesData(patchClient, patchConsignee, patchConsignor, patchNotifyParty,
        orgMapForRequests, addressMapForRequests, entity);

  }

  private void setAllPartiesData(
      Parties client,
      Parties consignee,
      Parties consigner,
      Parties notifyParty,
      Map<String, Map<String, Object>> orgMap,
      Map<String, Map<String, Object>> addressMap,
      ShipmentDetails entity
  ) {
    setPartyData(client, orgMap, addressMap, entity.getClient());
    setPartyData(consignee, orgMap, addressMap, entity.getConsignee());
    setPartyData(consigner, orgMap, addressMap, entity.getConsigner());
    if (!Objects.isNull(entity.getAdditionalDetails())) {
      Parties oldNotifyParty = entity.getAdditionalDetails().getNotifyParty();
      setPartyData(notifyParty, orgMap, addressMap, oldNotifyParty);
      entity.getAdditionalDetails().setNotifyParty(oldNotifyParty);
    }
  }

  private void setPartyData(Parties party, Map<String, Map<String, Object>> orgMap,
      Map<String, Map<String, Object>> addressMap, Parties entityParty) {
    if (!Objects.isNull(party)) {
      party.setOrgData(getOrgForParty(orgMap, party));
      party.setAddressData(getAddressForParty(addressMap, party));
      modelMapper.map(party, entityParty);
    }
  }


  private Map<String, Object> getAddressForParty(Map<String, Map<String, Object>> addressMap,
      Parties party) {
    return Optional.ofNullable(party)
        .map(parties -> parties.getOrgCode() + "#" + parties.getAddressCode())
        .map(addressMap::get)
        .orElse(null);
  }

  private Map<String, Object> getOrgForParty(Map<String, Map<String, Object>> orgMap,
      Parties party) {
    return Optional.ofNullable(party)
        .map(Parties::getOrgCode)
        .map(orgMap::get)
        .orElse(null);
  }

}
