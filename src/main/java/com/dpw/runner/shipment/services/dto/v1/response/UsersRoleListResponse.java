package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;
import java.io.Serializable;
@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UsersRoleListResponse implements Serializable {
    @JsonProperty("RoleId")
    private Long RoleId;
    @JsonProperty("Username")
    private String Username;
    @JsonProperty("Email")
    private String Email;
}