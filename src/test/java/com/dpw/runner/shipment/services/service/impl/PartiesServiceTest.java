package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.dto.patchRequest.ShipmentPatchRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PartiesServiceTest {

  @InjectMocks
  private PartiesService partiesService;

  @Mock
  private V1ServiceUtil v1ServiceUtil;

  @Mock
  private ModelMapper modelMapper;

  private Parties client;
  private Parties consignee;
  private Parties consigner;
  private Parties notifyParty;
  private OrgAddressResponse orgAddressResponse;


  @BeforeEach
  public void setUp() {
    client = Parties.builder().orgCode("ClientOrgCode").addressCode("AddressOrgCode").build();
    consignee = Parties.builder().orgCode("ConsigneeOrgCode").addressCode("ConsigneeOrgCode").build();
    consigner = Parties.builder().orgCode("ConsignerOrgCode").addressCode("ConsignerOrgCode").build();
    notifyParty = Parties.builder().orgCode("NotifyOrgCode").addressCode("NotifyOrgCode").build();
    orgAddressResponse = new OrgAddressResponse();

    Map<String, Map<String, Object>> addressMap = new HashMap<>();
    Map<String, Map<String, Object>> orgMap = new HashMap<>();
    orgMap.put("O1", new HashMap<>());
    orgMap.put("O2", new HashMap<>());

    addressMap.put("O1#A1", new HashMap<>());
    addressMap.put("O2#A2", new HashMap<>());

    orgAddressResponse.setAddresses(addressMap);
    orgAddressResponse.setOrganizations(orgMap);

    when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
  }

  @Test
  void testPartialUpdateParties_AllPartiesPresent() {
    ShipmentPatchRequest shipmentPatchRequest = ShipmentPatchRequest.builder()
        .client(PartiesRequest.builder().orgCode("O1").addressCode("A1").build())
        .consignee(PartiesRequest.builder().orgCode("O2").addressCode("A2").build())
        .consigner(PartiesRequest.builder().orgCode("O3").addressCode("A3").build())
        .notifyParty(PartiesRequest.builder().orgCode("O4").addressCode("A4").build())
        .build();

    ShipmentDetails shipmentDetails = new ShipmentDetails();
    shipmentDetails.setClient(client);
    shipmentDetails.setConsigner(consignee);
    shipmentDetails.setConsigner(consigner);
    AdditionalDetails additionalDetails = new AdditionalDetails();
    additionalDetails.setNotifyParty(notifyParty);
    shipmentDetails.setAdditionalDetails(additionalDetails);

    Parties client = new Parties();
    client.setOrgCode("O1");
    client.setAddressCode("A1");

    when(modelMapper.map(any(PartiesRequest.class), eq(Parties.class))).thenReturn(client);

    partiesService.partialUpdateParties(shipmentPatchRequest, shipmentDetails);
    assertEquals("ClientOrgCode", shipmentDetails.getClient().getOrgCode());
    assertEquals("AddressOrgCode", shipmentDetails.getClient().getAddressCode());

  }

}
