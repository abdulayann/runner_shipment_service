package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.enums.AddressTypes;
import com.dpw.runner.shipment.services.entitytransfer.enums.RegStatus;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferAddress implements IEntityTranferBaseEntity {

    @JsonProperty("Id")
    private long id;
    public Long OrgId;
    public String AddressShortCode;
    public String CompanyName;
    public AddressTypes AddressType;
    public String SiteIdentifier;
    public String AdditionalAddressInfo;
    public String Address1;
    public String Address2;
    public String Country;
    public String City;
    public String State;
    public String ZipPostCode;
    public String Unloco;
    public String ContactPhone;
    public String Mobile;
    public String Fax;
    public String Email;
    public Boolean Active;
    public Boolean IsObsolete;
    public Boolean IsEdited;
    public String locCode;
    public int CustomerType;
    public String OrgCode;
    public Boolean DefaultAddress;
    public String TaxRegNumber;
    public Boolean CustomClearanceLock;
    public String IECode;
    public String PartyName;
    public String BranchSINumber;
    public String ContactPerson;
    public String OrgPlaceOfSupply;
    public String StateCode;
    public String StateID;
    public RegStatus RegistrationStatus;
    public Boolean TaxExemptionApplicable;
    public LocalDateTime ExemptionStartDate;
    public LocalDateTime ExemptionExpiryDate;
    public String ExemptionSection;
    @JsonProperty("RegulatedAgent")
    private Boolean regulatedAgent;
    @JsonProperty("KnownConsignor")
    private Boolean knownConsignor;
    public String KCRANumber;
    public String KCRAExpiry;
}
